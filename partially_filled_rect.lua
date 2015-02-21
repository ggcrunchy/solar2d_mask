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
local ipairs = ipairs
local pairs = pairs
local remove = table.remove
local sort = table.sort

-- Modules --
local require_ex = require("tektite_core.require_ex")
local gray = require_ex.Lazy("number_sequences.gray")
local log2 = require_ex.Lazy("bitwise_ops.log2")
local mask = require("corona_utils.mask")
local table_funcs = require("tektite_core.table.funcs")

-- Cached module references --
local _NewSheet_

-- Exports --
local M = {}

--[=[
-- TILES, GENERATED BY BUILD TILES --
local Tiles = FROM

-- Tile count --
local NumTiles = #Tiles

-- --
local CellCache = {}

-- --
local Dirty = setmetatable({}, {
	__index = function(t, k)
		local new = remove(CellCache) or {}

		rawset(t, k, new)

		return new
	end
})

-- --
local DirtyN

--- DOCME
function M.GetCell ()
	DirtyN = DirtyN + 1

	return Dirty[DirtyN]
end

-- --
local Bx

--- DOCME
function M.Init (bx, by)
	Bx = bx
end

-- --
local Blocks = {}

--- DOCME
function M.NewImage (image, x, y, w, h, col, row)
	image:setFrame(FullIndex)

	image.x, image.y = x + NCOLS * 2, y + NROWS * 2
	image.width, image.height = w, h

	if row > 0 then
		Blocks[#Blocks + 1] = { col = (col - 1) * NCOLS + 1, row = (row - 1) * NROWS + 1, flags = (Mask + Mask) - 1, image = image }
	end
end

-- --
local BlocksProcessed = {}

--- DOCME
function M.UpdateBlocks ()
	-- Order the list, for easy detection of duplicates.
	sort(BlocksProcessed)

	-- Update the images belonging to each affected block.
	local prev_block

	for i = #BlocksProcessed, 1, -1 do
		local block_index = BlocksProcessed[i]

		if block_index ~= prev_block then
			local block = Blocks[block_index]
			local flags, image, prepped = block.flags, block.image

			-- Decompose the block until it matches a tile.
			while not Tiles[flags] and flags ~= 0 do
				local flag, index = 1, 1

				-- On the first iteration, prep the workspace. Reset the flags, which will be
				-- built up over this iteration.
				flags, prepped = 0, prepped or PrepWorkspace(flags)

				-- Remove any thin strips.
				for row = 1, NROWS do
					for col = 1, NCOLS do
						if Workspace[index] then
							local passed, lcheck, rcheck = Workspace[index - NCOLS] or Workspace[index + NCOLS]

							if passed then
								lcheck = col > 1 and Workspace[index - 1]
								rcheck = col < NCOLS and Workspace[index + 1]
							end

							if passed and (lcheck or rcheck) then
								flags = flags + flag
							else
								Workspace[index] = false
							end
						end

						flag, index = flag + flag, index + 1
					end
				end
			end

			-- Update the tile acoording to the new block flags.
			block.flags = flags

			if flags > 0 then
				image.isVisible = true

				image:setFrame(Tiles[flags])
				image.width = 16 -- shouldn't be our responsibility...
				image.height = 16
			else
				image.isVisible = false
			end

			prev_block = block_index
		end

		BlocksProcessed[i] = nil
	end
end

-- Cache any excess cells left over from previous processing
local function CacheExcess ()
	for i = #Dirty, DirtyN + 1, -1 do
		CellCache[#CellCache + 1], Dirty[i] = Dirty[i]
	end
end

--
local function IndexComp (a, b)
	return a.index < b.index
end

--
local function OnAcquireBlock_Func (block_index, func)
	func("block", block_index)
end

--
local function OnAcquireBlock_Process (block_index)
	BlocksProcessed[#BlocksProcessed + 1] = block_index
end

--
local function GetFlagsAndCheck (block, NON)
	local flags = block.flags

	if NON then
		return flags, block.NON_flags or 0
	else
		return flags, flags
	end
end

--
local function IsFilled (flags, fmask)
	return flags % (fmask + fmask) >= fmask
end

--
local function OnCell_Fill (block, col, row, fmask, NON)
	local flags, check = GetFlagsAndCheck(block, NON)

	if not IsFilled(check, fmask) then
		block.flags = flags + fmask

		if NON then
			block.NON_flags = check + fmask
		end
	end
end

--
local function OnCell_Func (block, col, row, fmask, func)
	func("cell", col, row, IsFilled(block.flags, fmask))
end

--
local function OnCell_Wipe (block, col, row, fmask, NON)
	local flags, check = GetFlagsAndCheck(block, NON)

	--
	if IsFilled(check, fmask) then
		block.flags = flags - fmask

		if NON then
			block.NON_flags = check - fmask
		else
			poof.DoPoof(col, row)
		end
	end
end

--- DOCME
function M.VisitCells (how, arg)
	CacheExcess()

	-- Order the list, for easy detection of duplicates.
	sort(Dirty, IndexComp)

	-- Choose the appropriate operations and argument.
	local on_acquire_block, on_cell

	if how == "fill" or how == "wipe" then
		on_acquire_block = OnAcquireBlock_Process
		on_cell = how == "fill" and OnCell_Fill or OnCell_Wipe
	else
		on_acquire_block, on_cell, arg = OnAcquireBlock_Func, OnCell_Func, how
	end

	-- Visit each unique index and compact the list.
	local clo, rlo, chi, rhi, block, prev = 0, 0, -1, -1

	for _, cell in ipairs(Dirty) do
		local index = cell.index

		if index ~= prev then
			local col, row = cell.col, cell.row

			--
			if col < clo or col > chi or row < rlo or row > rhi then
				local bcol, brow = floor(CFRAC * (col - 1)), floor(RFRAC * (row - 1))

				clo, rlo = bcol * NCOLS + 1, brow * NROWS + 1
				chi, rhi = clo + NCOLS - 1, rlo + NROWS - 1

				local block_index = brow * NBLOCKCOLS + bcol + 1

				block = Blocks[block_index]

				on_acquire_block(block_index, arg)
			end

			--
			local power = (row - rlo) * NCOLS + (col - clo)

			on_cell(block, col, row, 2^power, arg)

			prev = index
		end
	end
end

]=]

--
local function GetPixAndDim (opts, npix_name, dim_name)
	local dim = opts[dim_name] or opts.dim
	local npix = opts[npix_name] or opts.npix

	return npix, 1 / npix
end

-- Turns flags into a 2D grid 
local function PrepWorkspace (work, mask, nbits, flags)
	for i = nbits, 1, -1 do
		if flags >= mask then
			work[i], flags = true, flags - mask
		else
			work[i] = false
		end

		mask = .5 * mask
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


--- DOCME
-- @callable get_object
-- @uint w 
-- @uint h
-- @uint ncols
-- @uint nrows
-- @ptable[opt] opts
-- @todo ^^^ May need revision!
-- @treturn function F
-- @treturn function G
function M.NewGrid (get_object, w, h, ncols, nrows, opts)
	local sheet, data = _NewSheet_(opts)
	local work, map, mask, nbits, full_index = {}, ResolveData(data)
	local cols, cfrac, pixw = GetPixAndDim(opts, "npix_cols", "pixw")
	local rows, rfrac, pixh = GetPixAndDim(opts, "npix_rows", "pixh")
	-- ^^ This is where the 4's and .25's in the code originate (cols = 4, rows = 4)
	-- ^^ Not sure if pixw / pixh are useful (maybe to find scale?)

--[[
	local reel, clear, full = _NewReel_(dim, w / ncols, h / nrows, opts), Clear, Full
	local cleared, dirty_cells, ndirty, id = {}, {}, 0, 0
	local pitch, total = ncols + 2, (ncols + 2) * (nrows + 2)
	local correct = Correction(pitch, total, opts and opts.wrap)

	if opts and opts.flip_color then
		clear, full = Full, Clear
	end
]]
	return function(col, row, clear)
		--[[
		-- If a cell is dirtied, flip its state, then add each of the four affected display
		-- object cells (i.e. one per corner) to a dirty list. This is done implicitly, since all
		-- four can be rebuilt from a given corner; the column and row corresponding to the chosen
		-- corner are stored, in order to forgo some later recomputation.
		if col >= 1 and col <= ncols and row >= 1 and row <= nrows then
			local index = row * pitch + col + 1
			local not_clear = not cleared[index]

			if not_clear ~= not clear then
				dirty_cells[ndirty + 1] = index
				dirty_cells[ndirty + 2] = col
				dirty_cells[ndirty + 3] = row

				ndirty, cleared[index] = ndirty + 3, not_clear
			end
		end
		]]
	end, function(arg)
		--[[
		if ndirty > 0 then
			-- Visit each dirty cell, building up a state from the cell's corners.
			for i = 1, ndirty, 3 do
				local index, col, row = unpack(dirty_cells, i, i + 2)
				local cindex, ccol, crow = index, col, row

				for j = 1, 4 do
					if dirty_cells[-cindex] ~= id then
						local ul = cleared[correct(index - pitch - 1, col - 1, row - 1)] and UL or None
						local ur = cleared[correct(index - pitch, ccol, row - 1)] and UR or None
						local ll = cleared[correct(index - 1, col - 1, crow)] and LL or None
						local lr = cleared[cindex] and LR or None
						local state = ul + ur + ll + lr

						-- If all corners were cleared, a cell's object becomes invisible. Otherwise, the
						-- object remains (or becomes) visible.
						local object = get_object(ccol, crow, ncols, nrows, arg)

						if object then
							object.isVisible = state ~= clear

							-- If a cell is full, there is nothing to mask.
							if state == full then
								object:setMask(nil)

							-- Otherwise, apply the mask at the given frame. (For empty cells, it is enough
							-- that the object was made invisible.)
							elseif state ~= clear then
								reel("set", object, state)
							end
						end

						-- Mark the cell as visited.
						dirty_cells[-cindex] = id
					end

					-- Step to another corner.
					if j < 4 then
						local dc = 2 - j

						if dc ~= 0 then
							index, col = index + dc, col + dc
						else
							index, row = index + pitch, row + 1
						end

						--
						if correct ~= DefCorrect then
							cindex, ccol, crow = correct(index, col <= ncols, row <= nrows)
							ccol, crow = ccol or col, crow or row
						else
							cindex, ccol, crow = index, col, row
						end
					end
				end
			end

			-- Update the ID and invalidate one cell.
			id, ndirty, dirty_cells[-(id + 1)] = (id + 1) % total, 0	
		end
		]]
	end
end






--- DOCME
-- @ptable opts
-- @treturn MaskSheet MS
-- @return ARG
function M.NewSheet (opts)
	opts = table_funcs.Copy(opts)

	opts.name, opts.dimx, opts.dimy, opts.dim = "PartiallyFilledRect"

	local method = opts.get_data and "NewSheet_Data" or "NewSheet"
	local sheet, data = mask[method](opts), opts.data

	if not sheet:IsLoaded() then
		local ncols, pixw = opts.npix_cols or opts.npix, opts.pixw or opts.pix_dim
		local nrows, pixh = opts.npix_rows or opts.npix, opts.pixh or opts.pix_dim

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

		-- Checks if either neighbor is in use
		local function CheckEither (from, dir1, dir2)
			return in_use[from[dir1]] or in_use[from[dir2]]
		end

		-- If not just getting data, create the frame logic.
		local After, MakeFrame

		if method == "NewSheet" then
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
		local prev_gray, is_white, is_intact = 0, not not opts.flip_color

		for gval in gray.FirstN(2 ^ (ncols * nrows), 0) do -- skip 0
			-- Update Gray value-associated state.
			local diff, from, skip_test = gval - prev_gray
			local added = diff > 0
			local at = log2.Lg_Floor(added and diff or -diff) + 1

			from, in_use[at], prev_gray = neighbors[at], added, gval

			-- If a bit was removed, check whether any of the associated element's neighbors
			-- became (or already were) filaments. In that case, the pattern is not intact,
			-- so any integrity check would be redundant.
			if not added then
				for dir, next in pairs(from) do
					if in_use[next] and not in_use[neighbors[next][dir]] then
						skip_test, is_intact = true, false

						break
					end
					-- TODO: ^^^ Instead of breaking, increment filament count for each violation
				end

			-- Otherwise, if the pattern was intact on the previous step, either it remains
			-- so (i.e. the added element coaelesced with a larger region) or, at worst, a
			-- single-element filament is introduced. In either case, no integrity check is
			-- necessary. If the pattern was broken, on the other hand, proceed with it.
			elseif is_intact then
				skip_test = true
				is_intact = CheckEither(from, "up", "down") and CheckEither(from, "left", "right")
				-- TODO: Reduce filaments if possible
				-- TODO: Add filaments if necessary
			end

			-- Integrity check: ensure that no filaments exist. The pattern is considered to
			-- be intact when this condition is satisfied.
			-- TODO: There must be a way to do this incrementally with some counters and flags
			-- Gray code would still keep it sane; "is_intact", then, is when nfilaments = 0
			if not skip_test then
				is_intact = true

				for i, from in ipairs(neighbors) do
					if in_use[i] and not (CheckEither(from, "up", "down") and CheckEither(from, "left", "right")) then
						is_intact = false

						break
					end
				end
			end

			-- Intact pattern: submit its texels (or data) to the mask sheet.
			if is_intact then
				if method == "NewSheet" then
					sheet:AddFrame(MakeFrame, gval, is_white, After)
				else
					sheet:AddFrame(gval)
				end
			end
		end

		data = sheet:Commit()
	end

	return sheet, data
end

-- Cache module members.
_NewSheet_ = M.NewSheet

-- Export the module.
return M