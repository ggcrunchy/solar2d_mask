--- This module provides operations to generate and use rectangular, sheet-style masks, where
-- individual frames 
 
--
-- Permission is hereby granted, free of charge, to any person obtaining
-- a copy of this software and associated documentation files (the
-- "Software"), to deal in the Software without restriction, including
-- without limitation the rights to use, copy, modify, merge, publish,
-- distribute, sublicense, and/or sell copies of the Software, and to
-- permit persons to whom the Software is furnished to do so, subject to
-- the following conditions:
--
-- The above copyright notice and this permission notice shall be
-- included in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
-- IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
-- CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
-- TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
-- SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
--
-- [ MIT license: http://www.opensource.org/licenses/mit-license.php ]
--

-- Standard library imports --
local floor = math.floor
local ipairs = ipairs
local remove = table.remove

-- Modules --
local require_ex = require("tektite_core.require_ex")
local gray = require_ex.Lazy("number_sequences.gray")
local grid_funcs = require("tektite_core.array.grid")
local mask = require("corona_utils.mask")
local match_slot_id = require("tektite_core.array.match_slot_id")
local table_funcs = require("tektite_core.table.funcs")

-- Cached module references --
local _NewSheet_

-- Exports --
local M = {}

--
local function OnAcquireBlock_Func (block_index, func)
	func("block", block_index)
end

--
local function OnAcquireBlock_Process (block_index, processed)
	processed[#processed + 1] = block_index
end

--
local function GetFlagsAndCheck (block, alt)
	local flags = block.flags

	if alt then
		return flags, block.alt_flags or 0
	else
		return flags, flags
	end
end

--
local function IsFilled (flags, fmask)
	return flags % (2 * fmask) >= fmask
end

--
local function OnCell_Fill (block, col, row, fmask, alt, action)
	local flags, check = GetFlagsAndCheck(block, alt)

	if not IsFilled(check, fmask) then
		block.flags = flags + fmask

		if alt then
			block.alt_flags = check + fmask
		end

		if action then
			action("fill", col, row, alt)
		end
	end
end

--
local function OnCell_Func (block, col, row, fmask, func)
	func("cell", col, row, IsFilled(block.flags, fmask))
end

--
local function OnCell_Wipe (block, col, row, fmask, alt, action)
	local flags, check = GetFlagsAndCheck(block, alt)

	if IsFilled(check, fmask) then
		block.flags = flags - fmask

		if alt then
			block.alt_flags = check - fmask
		end

		if action then
			action("wipe", col, row, alt)
		end
	end
end

--
local function GetCountsAndPix (reader, count_name, npix_spr_name)
	local npix = reader(npix_spr_name)

	return reader(count_name), npix, 1 / npix
end

-- Turns flags into a 2D grid 
local function PrepWorkspace (work, flags)
	local cur_mask = work.mask

	for i = work.nbits, 1, -1 do
		if flags >= cur_mask then
			work[i], flags = true, flags - cur_mask
		else
			work[i] = false
		end

		mask = .5 * cur_mask
	end

	return true
end

-- Converts the array to a map, finds the largest mask / bit count, and gets the "full" mask index
local function ResolveData (data)
	-- Invert the array, finding the "full" mask along the way.
	local new, full = {}, -1

	for i, index in ipairs(data) do
		if index > full then
			full = index
		end

		new[index] = i
	end

	-- Find the "full" mask and associated bit count.
	local full_mask, nbits, next_mask = 1, 1, 2

	repeat
		full_mask, next_mask, nbits = next_mask, 2 * next_mask, nbits + 1
	until next_mask > full

	return new, full_mask, nbits, new[full]
end

--
local function UpdateBlocks (blocks, processed, work, map, sheet, get_object)
	-- Order the list, for easy detection of duplicates.
--	sort(processed)

	-- Update the images belonging to each affected block.
--	local prev_block

	local pix_cols, pix_rows = blocks.pix_cols, blocks.pix_rows

	for _ = 1, #processed do
		local block_index = remove(processed)

	--	if block_index ~= prev_block then
			local block = blocks[block_index]
			local flags, prepped = block.flags

			-- Decompose the block until it matches a tile.
			while not map[flags] and flags ~= 0 do
				-- On the first iteration, prep the workspace. Reset the flags, which will be
				-- built up over this iteration.
				flags, prepped = 0, prepped or PrepWorkspace(work, flags)

				-- Remove any filaments.
				local flag, index = 1, 1

				for _ = 1, pix_rows do -- NROWS (pixrows)
					for col = 1, pix_cols do -- NCOLS (pixcols)
						if work[index] then
							local passed = work[index - pix_cols] or work[index + pix_cols] -- NCOLS (pixcols)

							if passed and ((col > 1 and work[index - 1]) or (col < pix_cols and work[index + 1])) then
								flags = flags + flag
							else
								work[index] = false
							end
						end

						flag, index = 2 * flag, index + 1
					end
				end
			end

			-- Update the tile acoording to the new block flags.
			block.flags = flags

			local object = get_object()--ccol, crow, ncols, nrows, arg) <- block.col, block.row, ncols (blockcols), nrows (blockrows), arg

			if object then
				sheet:Set(object, map[flags])

				-- image.width = 16 -- shouldn't be our responsibility...
				-- image.height = 16
			end

		--	prev_block = block_index
	--	end
	end
end

--
local function FracRem (frac, count, num)
	local quot = floor(frac * (num - 1))

	return num, quot, num - (quot * count + 1)
end

--
local function VisitCells (blocks, block_wrap, cells, ncells, processed, how, action, alt)
	-- Choose the appropriate operations and argument.
	local on_acquire_block, on_cell, ab_arg, cell_arg

	if how == "fill" or how == "wipe" then
		on_acquire_block = OnAcquireBlock_Process
		on_cell = how == "fill" and OnCell_Fill or OnCell_Wipe
		ab_arg, cell_arg = processed, alt
	else
		on_acquire_block, on_cell = OnAcquireBlock_Func, OnCell_Func
		ab_arg, cell_arg = how, how
	end

	-- Visit each unique index and compact the list. <- TODO: Rewrite
--	local clo, rlo, chi, rhi, block, prev = 0, 0, -1, -1
	local pix_cols, pix_rows = blocks.pix_cols, blocks.pix_rows
	local cfrac, rfrac = cells.cfrac, cells.rfrac
	local ncols, mask = blocks.ncols, blocks.mask

	for i = 1, ncells, 2 do
		local col, bcol, coff = FracRem(cfrac, pix_cols, cells[i])
		local row, brow, roff = FracRem(rfrac, pix_rows, cells[i + 1])
		local bindex = brow * ncols + bcol + 1

		if block_wrap("mark", bindex) then
			local block = blocks[bindex] or { col = (col - 1) * pix_cols + 1, row = (row - 1) * pix_rows + 1, flags = mask } -- factors necessary?

			blocks[bindex] = block

			on_acquire_block(bindex, ab_arg)
		end
--[[
		--
		if col < clo or col > chi or row < rlo or row > rhi then
			local bcol, brow = floor(cfrac * (col - 1)), floor(rfrac * (row - 1))

			clo, rlo = bcol * cols + 1, brow * rows + 1 -- NCOLS, NROWS (pixdims)
			chi, rhi = clo + cols - 1, rlo + rows - 1 -- NCOLS, NROWS (pixdims)

			local block_index = brow * ncols + bcol + 1 -- NBLOCKCOLS (blockcols)
-- GetBlock(blocks, block_index, ncols, nrows, work)
			block = blocks[block_index]

			on_acquire_block(block_index, ab_arg)
		end
]]
		-- ^^^ Argh, without sorting, this is broken :/
		-- However, could just do another match-slot-id

		--
		local power = --[[(row - rlo)]]roff * pix_cols + coff--(col - clo) -- NCOLS (pixcols)

		on_cell(blocks[bindex], col, row, 2^power, cell_arg, action)
	end
end

--- DOCME
-- @callable get_object
-- @ptable[opt] opts
-- @treturn function F
-- @treturn function G
function M.NewGrid (get_object, opts)
	--
	local sheet, reader = _NewSheet_(opts)
	local map, mask, nbits, full_index = ResolveData(sheet:GetData())
	local bcols, pix_cols, cfrac = GetCountsAndPix(reader, "ncols", "npix_sprite_cols")
	local brows, pix_rows, rfrac = GetCountsAndPix(reader, "nrows", "npix_sprite_rows")
	local ncols, nrows = bcols * pix_cols, brows * pix_rows

-- get_object...
	--
	local nblocks = ncols * nrows
	local ncells, cell_wrap, cells = 0, match_slot_id.Wrap({
		cfrac = cfrac, rfrac = rfrac
	}, nblocks * (ncols * nrows))
	local block_wrap, blocks = match_slot_id.Wrap({
		pix_cols = pix_cols, pix_rows = pix_rows, ncols = ncols, mask = 2 * mask + 1
	}, nblocks)
	local processed, work = {}, { nbits = nbits }

	--
	if opts.flip_color then
		sheet:BindPatterns(map[full_index], 0)
	else
		sheet:BindPatterns(0, map[full_index])
	end
print("BCOLS, PIXCOLS", bcols, pix_cols)
print("BROWS, PIXROWS", brows, pix_rows)
print("NCOLS, NROWS", ncols, nrows)
print("")
	--
	local check_grid = grid_funcs.GridChecker_Blocks(16, 16, bcols, brows, pix_cols, pix_rows)

	return function(col, row)
		-- If a cell is dirtied, flip its state, then add each of the four affected display
		-- object cells (i.e. one per corner) to a dirty list. This is done implicitly, since all
		-- four can be rebuilt from a given corner; the column and row corresponding to the chosen
		-- corner are stored, in order to forgo some later recomputation.


		if col >= 1 and col <= ncols and row >= 1 and row <= nrows then -- pixdims
			if cell_wrap("mark", (row - 1) * ncols + col) then -- pixcols
				cells[ncells + 1], cells[ncells + 2], ncells = col, row, ncells + 2
			end
		end
	end, function(how, action, alt)
		if ncells > 0 then
			VisitCells(blocks, block_wrap, cells, ncells, processed, how, action, alt)
			UpdateBlocks(blocks, processed, work, map, sheet, get_object)

			cell_wrap("begin_generation")
			block_wrap("begin_generation")

			ncells = 0
		end
	end
end

--- DOCME
-- @ptable opts
-- @treturn MaskSheet MS
-- @treturn function READER
-- @return ARG
function M.NewSheet (opts)
	opts = table_funcs.Copy(opts)

	opts.name, opts.dimx, opts.dimy, opts.dim = "PartiallyFilledRect"

	local method = opts.get_data and "NewSheet_Data" or "NewSheet_Grid"
	local sheet, reader = mask[method](opts), mask.NewReader(opts)

	if not sheet:IsLoaded() then
		local ncols, pixw = reader("npix_cols"), reader("pixw")
		local nrows, pixh = reader("npix_rows"), reader("pixh")

		-- Begin with all elements unused. Store neighbor indices for quick lookup.
		local in_use, neighbors, ni = {}, {}, 1

		for row = 1, nrows do
			local above, below = row > 1, row < nrows

			for col = 1, ncols do
				neighbors[ni], ni = {
					up = above and ni - ncols,
					left = col > 1 and ni - 1,
					right = col < ncols and ni + 1,
					down = below and ni + ncols
				}, ni + 1
			end
		end

		-- Checks if both neighbors are in use
		local function CheckBoth (from, dir1, dir2)
			return in_use[from[dir1]] and in_use[from[dir2]]
		end

		-- If not just getting data, create the frame logic.
		local After, MakeFrame

		if method == "NewSheet_Grid" then
			-- After function: clean up stash
			function After (cgroup)
				for i = cgroup.numChildren, 1, -1 do
					sheet:StashRect(cgroup[i])
				end
			end

			-- Frame factory
			function MakeFrame (cgroup, fg)
				local ci, y = 1, 0

				for row = 1, nrows do
					local inner_row, x = row > 1 and row < nrows, 0

					for col = 1, ncols do
						if in_use[ci] then
							local around = neighbors[ci]
							local on_edge = inner_row and not CheckBoth(around, "up", "down")

							on_edge = on_edge or (col > 1 and col < ncols and not CheckBoth(around, "left", "right"))

							sheet:GetRect(cgroup, x, y, pixw, pixh, on_edge and .65 or fg)
						end

						ci, x = ci + 1, x + pixw
					end

					y = y + pixh
				end
			end
		end

		-- Examine all possible patterns defined by a bit stream (where each bit indicates an
		-- "off" or "on" element), accepting any without "filaments", i.e. elements that lack
		-- either a horizontal or vertical neighbor (or both). Iterating this stream in Gray
		-- code order maintains pattern coherency, which simplifies update handling.
		local nviolations, is_white = 0, not not opts.flip_color

		for gval, at, added in gray.FirstN_Change(2 ^ (ncols * nrows) - 1) do
			in_use[at] = added

			-- If a bit was removed, check whether any of the associated element's neighbors became
			-- filaments, incrementing the violation count accordingly. If none were found, there are
			-- three possible scenarios: the element was on the edge of a "thick" slab; it was between
			-- two such slabs; or it was isolated. In the third case, where the element itself was a
			-- filament, the violation count is reduced.

			-- The situations are reversed when a bit is added. If the corresponding element coalesced
			-- with other elements, filaments may have become slabs, thus decrementing the violation
			-- count. Otherwise, the new element being a filament, the count is increased.
			local from, dir1, dir2, inc = neighbors[at], "left", "right", added and 1 or -1

			for _ = 1, 2 do
				local n1, n2 = from[dir1], from[dir2]
				local u1, u2 = in_use[n1], in_use[n2]

				if u1 or u2 then
					if u1 and not in_use[neighbors[n1][dir1]] then
						nviolations = nviolations - inc
					end

					if u2 and not in_use[neighbors[n2][dir2]] then
						nviolations = nviolations - inc
					end
				else
					nviolations = nviolations + inc
				end

				dir1, dir2 = "up", "down"
			end

			-- No violations: submit filament-free pattern (or data) to the mask sheet.
			if nviolations == 0 then
				if method == "NewSheet_Grid" then
					sheet:AddFrame(MakeFrame, gval, is_white, After)
				else
					sheet:AddFrame(gval)
				end
			end
		end

		sheet:Commit()
	end

	return sheet, reader
end

-- Cache module members.
_NewSheet_ = M.NewSheet

-- Export the module.
return M