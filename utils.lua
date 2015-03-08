--- Mask-building utilities.

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

-- Modules --
local require_ex = require("tektite_core.require_ex")
local gray = require_ex.Lazy("number_sequences.gray")

-- Corona globals --
local display = display

-- Cached module references --
local _NewRect_

-- Exports --
local M = {}

--- DOCMEMORE
-- Helper for black regions of mask texture
function M.BlackRect (group, stash, x, y, w, h)
	return _NewRect_(group, stash, x, y, w, h, 0)
end

-- Add 3 pixels to each side, then add (4 - 1) to round up to next multiple of 4 --
local Rounding = 3 * 2 + 3

--- DOCMEMORE
-- Helper to get extra padding and report odd counts
function M.Extra (n)
	local padding = Rounding - (n + Rounding) % 4
	local odd = padding % 2

	return (padding - odd) / 2, odd
end

--- DOCME
function M.GenerateFrames (sheet, ncols, nrows, in_use, neighbors, make_frame, after, is_white)
	-- Examine all possible patterns defined by a bit stream (where each bit indicates an "off"
	-- or "on" element), accepting any without "filaments", i.e. elements that lack either a
	-- horizontal or vertical neighbor (or both). Iterating this stream in Gray code order
	-- maintains pattern coherency, which simplifies update handling.
	local nviolations = 0

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
			if make_frame then
				sheet:AddFrame(make_frame, gval, is_white, after)
			else
				sheet:AddFrame(gval)
			end
		end
	end
end

--- DOCME
-- @uint ncols
-- @uint nrows
-- @treturn array
function M.GetNeighborList (ncols, nrows)
	local neighbors, ni = {}, 1

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

	return neighbors
end

--- DOCMEMORE
-- Common display rect constructor
function M.NewRect (group, stash, x, y, w, h, color)
	local n, rect = stash and stash.numChildren or 0

	if n > 0 then
		rect = stash[n]

		rect.width, rect.height = w, h

		group:insert(rect)
	else
		rect = display.newRect(group, 0, 0, w, h)
	end

	rect.anchorX, rect.x = 0, x
	rect.anchorY, rect.y = 0, y

	if color then
		rect:setFillColor(color)
	end

	return rect
end

--- DOCMEMORE
-- Find the "full" mask and associated bit count.
function M.NewWorkspace (full)
	local full_mask, nbits, next_mask = 1, 1, 2

	repeat
		full_mask, next_mask, nbits = next_mask, 2 * next_mask, nbits + 1
	until next_mask > full

	return { mask = full_mask, nbits = nbits }
end

--- DOCME
function M.PutRects (sheet, cgroup, ncols, nrows, pixw, pixh, in_use, color)
	local ci, y = 1, 0

	for row = 1, nrows do
		local inner_row, x = row > 1 and row < nrows, 0

		for col = 1, ncols do
			if in_use[ci] then
				sheet:GetRect(cgroup, x, y, pixw, pixh, color)
			end

			ci, x = ci + 1, x + pixw
		end

		y = y + pixh
	end
end

--- DOCME
function M.PutRects_Edges (sheet, cgroup, ncols, nrows, pixw, pixh, neighbors, in_use, color, edge_color)
	local ci, y = 1, 0

	for row = 1, nrows do
		local inner_row, x = row > 1 and row < nrows, 0

		for col = 1, ncols do
			if in_use[ci] then
				local around, cur_color = neighbors[ci], color

				if inner_row and not (in_use[around.up] and in_use[around.down]) then
					cur_color = edge_color
				elseif col > 1 and col < ncols and not (in_use[around.left] and in_use[around.right]) then
					cur_color = edge_color
				end

				sheet:GetRect(cgroup, x, y, pixw, pixh, cur_color)
			end

			ci, x = ci + 1, x + pixw
		end

		y = y + pixh
	end
end

--
local function PrepareWorkspace (work, flags)
	local mask = work.mask

	for i = work.nbits, 1, -1 do
		if flags >= mask then
			work[i], flags = true, flags - mask
		else
			work[i] = false
		end

		mask = .5 * mask
	end
end

--- DOCMEMORE
-- Decompose the block until it matches a frame.
function M.ResolveFlagsToFrame (map, work, flags, ccols, crows)
	local prepped

	while not map[flags] and flags ~= 0 do
		-- On the first iteration, prepare the workspace.
		if not prepped then
			PrepareWorkspace(work, flags)

			prepped = true
		end

		-- Reset the flags, which will be built up over this iteration.
		flags = 0

		-- Remove any filaments.
		local flag, index = 1, 1

		for _ = 1, crows do
			for col = 1, ccols do
				if work[index] then
					local passed = work[index - ccols] or work[index + ccols]

					if passed and ((col > 1 and work[index - 1]) or (col < ccols and work[index + 1])) then
						flags = flags + flag
					else
						work[index] = false
					end
				end

				flag, index = 2 * flag, index + 1
			end
		end
	end

	return flags
end

--- DOCME
function M.StashRects (sheet, cgroup)
	for i = cgroup.numChildren, 1, -1 do
		sheet:StashRect(cgroup[i])
	end
end

-- Cache module members.
_NewRect_ = M.NewRect

-- Export the module.
return M