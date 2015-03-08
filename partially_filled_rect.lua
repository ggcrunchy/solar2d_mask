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
local remove = table.remove

-- Modules --
local grid_funcs = require("tektite_core.array.grid")
local mask_sheet = require("corona_mask.sheet")
local mask_utils = require("corona_mask.utils")
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
local function OnCell_Update (block, col, row, fmask, alt, action, set)
	local flags, check = GetFlagsAndCheck(block, alt)

	if IsFilled(check, fmask) ~= set then
		fmask = set and fmask or -fmask

		block.flags = flags + fmask

		if alt then
			block.alt_flags = check + fmask -- These probably should be binary ops...
		end

		if action then
			action("update", col, row, set, alt)
		end
	end
end

--
local function OnCell_Func (block, col, row, fmask, func)
	func("cell", col, row, IsFilled(block.flags, fmask))
end

-- Converts the array to a map, finds the largest mask / bit count, and gets the "full" mask index
local function ResolveData (data)
	local new, full, fi = {}, -1, 1

	for i = 1, #data / 3 do
		local index = data[fi]

		if index > full then
			full = index
		end

		new[index], fi = i, fi + 3
	end

	return new, full
end

--
local function ChooseArg (opts, key)
	local arg = opts[key]

	if arg ~= nil then
		return arg
	else
		return opts.arg
	end
end

--
local function FracRem (frac, cells_per_block, coord)
	local quot = floor(frac * (coord - 1))

	return coord, quot + 1, coord - (quot * cells_per_block + 1)
end

-- Update the mask belonging to each affected block
local function UpdateBlocks (blocks, processed, work, map, sheet)
	local ccols, crows = blocks.ccols, blocks.crows

	for _ = 1, #processed do
		local block = blocks[remove(processed)]
		local flags = mask_utils.ResolveFlagsToFrame(map, work, block.flags, ccols, crows)

		block.flags = flags

		sheet:Set_Cell(block.col, block.row, flags)
	end
end

--
local function VisitCells (blocks, block_wrap, cells, ncells, processed, opts)
	-- Choose the appropriate operation and argument.
	local on_acquire_block, on_cell, ab_arg, cell_arg, action, alt

	if opts then
		on_acquire_block, on_cell = opts.on_acquire_block, opts.on_cell
		action, alt = opts.action, opts.alt
	end

	if on_acquire_block or on_cell then
		on_acquire_block, on_cell = OnAcquireBlock_Func, OnCell_Func
		ab_arg, cell_arg = ChooseArg(opts, "ab_arg"), ChooseArg(opts, "cell_arg")
	else
		on_acquire_block, on_cell = OnAcquireBlock_Process, OnCell_Update
		ab_arg, cell_arg = processed, alt
	end

	-- Visit each cell, performing some acquire logic on each unique block along the way.
	local ccols, crows = blocks.ccols, blocks.crows
	local cfrac, rfrac = cells.cfrac, cells.rfrac
	local bcols, bmask = blocks.bcols, blocks.mask

	for i = 1, ncells, 3 do
		local col, bcol, coff = FracRem(cfrac, ccols, cells[i])
		local row, brow, roff = FracRem(rfrac, crows, cells[i + 1])
		local bindex = (brow - 1) * bcols + bcol

		if block_wrap("mark", bindex) then
			local block = blocks[bindex] or { col = bcol, row = brow, flags = bmask }

			blocks[bindex] = block

			if on_acquire_block then
				on_acquire_block(bindex, ab_arg)
			end
		end

		--
		if on_cell then
			on_cell(blocks[bindex], col, row, 2^(roff * ccols + coff), cell_arg, action, cells[i + 2])
		end
	end
end

--- DOCME
-- @ptable[opt] opts
-- @treturn function F
-- @treturn function G
-- @treturn ?function H
function M.NewGrid (opts)
	--
	local sheet, reader = _NewSheet_(opts)
	local map, full = ResolveData(sheet:GetData())
	local bcols, ccols, cellw = reader("grid_ncols"), reader("cell_ncols"), reader("sprite_w")
	local brows, crows, cellh = reader("grid_nrows"), reader("cell_nrows"), reader("sprite_h")

	--
	local processed, work = {}, mask_utils.NewWorkspace(full)
	local ncells, cells = 0, { cfrac = 1 / ccols, rfrac = 1 / crows }
	local block_wrap, blocks = match_slot_id.Wrap({
		ccols = ccols, crows = crows, bcols = bcols, mask = 2 * work.mask - 1
	}, bcols * brows)

	--
	local full_index = map[full]

	if opts.flip_color then
		sheet:BindPatterns(map[full_index], 0)
	else
		sheet:BindPatterns(0, map[full_index])
	end

	--
	local get_cell = opts.get_cell

	if get_cell == "blocks" then
		get_cell = grid_funcs.GridChecker_Blocks(bcols * cellw, brows * cellh, bcols, brows, ccols, crows)
	elseif get_cell == "grid" then
		get_cell = grid_funcs.GridChecker(cellw / ccols, cellh / crows, bcols * ccols, brows * crows)
	else
		get_cell = nil
	end

	--
	local ncols, nrows = bcols * ccols, brows * crows

	return function(col, row, how)
		if col >= 1 and col <= ncols and row >= 1 and row <= nrows then
			cells[ncells + 1], cells[ncells + 2], cells[ncells + 3], ncells = col, row, how ~= "clear", ncells + 3
		end
	end, function(opts)
		if ncells > 0 then
			VisitCells(blocks, block_wrap, cells, ncells, processed, opts)
			UpdateBlocks(blocks, processed, work, map, sheet)

			block_wrap("begin_generation")

			ncells = 0
		end
	end, get_cell
end

--- DOCME
-- @ptable opts
-- @treturn MaskSheet MS
-- @treturn function READER
-- @return ARG
function M.NewSheet (opts)
	opts = table_funcs.Copy(opts)

	opts.name, opts.dimx, opts.dimy, opts.dim = "PartiallyFilledRect"
-- TODO: What were the "dim?" fields?
	local method = opts.get_data and "NewSheet_Data" or "NewSheet_Grid"
	local sheet, reader = mask_sheet[method](opts), mask_sheet.NewReader(opts)

	if not sheet:IsLoaded() then
		local ncols, pixw = reader("cell_ncols"), reader("frame_unit_w")
		local nrows, pixh = reader("cell_nrows"), reader("frame_unit_h")

		-- Begin with all elements unused. Store neighbor indices for quick lookup.
		local in_use, neighbors = {}, mask_utils.GetNeighborList(ncols, nrows)

		-- Checks if both neighbors are in use
		local function CheckBoth (from, dir1, dir2)
			return in_use[from[dir1]] and in_use[from[dir2]]
		end

		-- If not just getting data, create the frame logic.
		local After, MakeFrame, is_white

		if method == "NewSheet_Grid" then
			is_white = not not opts.flip_color

			-- After function: clean up stash
			function After (cgroup)
				mask_utils.StashRects(sheet, cgroup)
			end

			-- Frame factory
			function MakeFrame (cgroup, fg)
				mask_utils.PutRects_Edges(sheet, cgroup, ncols, nrows, pixw, pixh, neighbors, in_use, fg, .65)
			end
		end

		-- Generate frame data and load it into the sheet.
		mask_utils.GenerateFrames(sheet, ncols, nrows, in_use, neighbors, MakeFrame, After, is_white)

		sheet:Commit()
	end

	return sheet, reader
end

-- Cache module members.
_NewSheet_ = M.NewSheet

-- Export the module.
return M