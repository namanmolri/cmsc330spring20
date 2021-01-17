def value_breakdown(file)
  line = file.gets
  if line == nil then return end

  # read 1st line, must be maze header
  size, sx, sy, ex, ey = line.split(/\s/)
  norm_x = []
  norm_y = []
  dist= []
  weights= []
  path_name= []
  path_x= []
  path_y= []
  dist_arr= []
  # read additional lines
  while line = file.gets do

   if line[0...4] == "path"
    p, name, x, y, ds = line.split(/\s/)
    path_name.push(name)
    path_x.push(x)
    path_y.push(y)
    dist_arr.push(ds)
   else
    x, y, ds, w = line.split(/\s/,4)
    norm_x.push(x)
    norm_y.push(y)
    dist.push(ds)
    weights.push(w)
    ws = w.split(/\s/)
   end
  end
  return size,norm_x,norm_y,dist,weights,path_name, path_x,path_y,dist_arr
end


#-------------------------------------------------------

def bridge(maze_file)
    size, x, y, ds, w = value_breakdown(maze_file)
    size = Integer(size)
    count = 0
    i = 0
    while i < size do
        j = 0
        while j < size
            if ds[(size*i)+j] =~ /d/ && ds[(size*i)+j+1] =~ /[ud]\w*[ud]/ &&
                ds[(size*i)+j+2] =~ /u/ && (size*i)+j+2 < size*size then
                count +=1
            end
            if ds[i+(size*j)] =~ /r/ && ds[i+(size*(j+1))] =~ /[lr]\w*[rl]/ &&
                ds[i+(size*(j+2))] =~ /l/ && i+(size*(j+2)) < size*size then
                count +=1
            end
            j +=1
        end
        i +=1
    end
    return count
end


#-------------------------------------------------------

def open_cell(file)
  count = 0
  line = file.gets
  if line == nil then return end

  while line = file.gets do

    if line[0...4] == "path"
    else
      x, y, ds, w = line.split(/\s/,4) # since ds is u,l,d,r

      if ds != nil && ds.match(/\w{4}/)
        count += 1
      end
    end
  end
  return count
end



#-------------------------------------------------------

def sortcells(file)
  coord = {}
  arr0 = []
  arr1 = []
  arr2 = []
  arr3 = []
  arr4 = []
  line = file.gets
  if line == nil then return end
  size, sx, sy, ex, ey = line.split(/\s/)
  size = size.to_i - 1
  while line = file.gets do

    if line[0...4] == "path"
    else
      x, y, ds, w = line.split(/\s/,4)
      coord[[x.to_i,y.to_i]] = ds

    end
  end
  for x in (0..size)
    for y in (0..size)
      if coord[[x,y]] == nil || coord[[x,y]] == ""
        arr0 << [x,y]
      end
      if coord[[x,y]].match(/^\w{1}$/)
        arr1 << [x,y]
      end
      if coord[[x,y]].match(/^\w{2}$/)
        arr2 << [x,y]
      end
      if coord[[x,y]].match(/^\w{3}$/)
        arr3 << [x,y]
      end
      if coord[[x,y]].match(/^\w{4}$/)
        arr4 << [x,y]
      end
    end
  end
  str = []
  if arr0[0] != nil
    strs = "0"
    arr0.each{|arr| strs += ",(#{arr[0]},#{arr[1]})"}
    str << strs
  end
  if arr1[0] != nil
    strs = "1"
    arr1.each{|arr| strs += ",(#{arr[0]},#{arr[1]})"}
    str << strs
  end
  if arr2[0] != nil
    strs = "2"
    arr2.each{|arr| strs += ",(#{arr[0]},#{arr[1]})"}
    str << strs
  end
  if arr3[0] != nil
    strs = "3"
    arr3.each{|arr| strs += ",(#{arr[0]},#{arr[1]})"}
    str << strs
  end
  if arr4[0] != nil
    strs = "4"
    arr4.each{|arr| strs += ",(#{arr[0]},#{arr[1]})"}
    str << strs
  end
end


#-------------------------------------------------------

def pretty_print(file)

  result = path(file)

  file.seek 0
  h = {}
  paths = []
  shortest = []
  line = file.gets
  if line == nil then return end

  size, sx, sy, ex, ey = line.split(/\s/)
  size = size.to_i - 1
  start = [sx.to_i,sy.to_i]
  ending = [ex.to_i ,ey.to_i]
  while line = file.gets do

    if line[0...4] == "path"
      p, name, x, y, ds = line.split(/\s/)
      paths << [name,x.to_i,y.to_i,ds]
    else
      x, y, ds, w = line.split(/\s/,4)
      if ds != "" && ds != nil
        h[[x.to_i,y.to_i]] = ds
      end
    end
  end


  hash_new = {}
  if result != "none"
    name = result[0][11..(result[0].length - 1)]

    paths.each do |arr|
      if arr[0] == name
        p = arr
      end
    end
    d = p[3].split("")
    x = p[1]
    y = p[2]
    d.each do |w| 
      if w == "u"
        hash_new[[x,y]] = "u"
        y -= 1
      elsif w == "d"
        hash_new[[x,y]] = "d"
        y += 1

      elsif w == "r"
        hash_new[[x,y]] = "r"
        x += 1

      elsif w == "l"
        hash_new[[x,y]] = "l"
        x -= 1
      end

    end
    hash_new[[x,y]] = "e"
  end

  final = ""
  str = "+"
  for x in (0..size)
    str += "-+"
  end
  final += str
  final += "\n"
  for y in (0..size)
    string1 = "|"
    string2 = "+"
    for x in (0..size)

      if [x,y] == start && hash_new[[x,y]] != nil
        string1 += "S"
      elsif [x,y] == ending && hash_new[[x,y]] != nil
        string1 += "E"
      elsif [x,y] == start
        string1 += "s"
      elsif [x,y] == ending
        string1 += "e"
      elsif hash_new[[x,y]] != nil
        string1 += "*"
      else
        string1 += " "
      end

      if h[[x,y]] == nil
        string2 += "-+"
        string1 += "|"
      else
        if  h[[x,y]].match(/[d]/)
          string2 +=  " +"
        else
          string2 += "-+"
        end

        if h[[x,y]].match(/[r]/)
          string1 +=  " "
        else
          string1 +=  "|"
        end

      end


    end
    if y == size
      final += string1
      final += "\n"
      final += string2
    else
      final += string1
      final += "\n"
      final += string2
      final += "\n"
    end
  end
  return final
end


#-------------------------------------------------------

  def path(file)
    paths = []
    m = {}
    final = []
    weight = []
    m2= {}
    line = file.gets
    if line == nil then return end
    size, sx, sy, ex, ey = line.split(/\s/)
    size = size.to_i - 1
    while line = file.gets do
  
      if line[0...4] == "path"
        p, name, x, y, ds = line.split(/\s/)
        paths << [name,x.to_i,y.to_i,ds]
      else
        x, y, ds, w = line.split(/\s/,4)
        if ds != "" && ds != nil && w != "" && w != nil
          ws = w.split(/\s/)
          m[[x.to_i,y.to_i]] = [ds, ws]
        end
  
  
      end
    end
    if paths[0] != nil
      paths.each do |arr|
        if checker(arr,m) != nil
          w = checker(arr,m)
          str = " "
          str += arr[0]
          weight << w
          m2[w] = str
        end
      end
    end
    if weight[0] != nil
      weight = weight.sort
  
      weight.each do |w|
        str = "%10.4f" % w
        str += m2[w]
        final << str
      end
    end
  
    if final[0] == nil
      s = "none"
    else
      final
    end
  end
  
  def checker(paths,m)
    x = paths[1]
    y = paths[2]
    d = []
    weight = 0
    tf = true
    if paths[3] != nil
      d = paths[3].split("")
  
      d.each do |w|
        if w == "u"
          if m[[x,y]] != nil && m[[x,y]][0].match(/[u]/)
            i = m[[x,y]][0]=~/[u]/
  
            weight += m[[x,y]][1][i].to_f
            y -= 1
          else
            tf = false
          end
        end
        if w == "d"
          if m[[x,y]] != nil && m[[x,y]][0].match(/[d]/)
            i = m[[x,y]][0]=~/[d]/
            weight += m[[x,y]][1][i].to_f
            y += 1
          else
            tf = false
          end
        end
        if w == "r"
          if m[[x,y]] != nil && m[[x,y]][0].match(/[r]/)
            i = m[[x,y]][0]=~/[r]/
            weight += m[[x,y]][1][i].to_f
            x += 1
          else
            tf = false
          end
        end
        if w == "l"
          if m[[x,y]] != nil && m[[x,y]][0].match(/[l]/)
            i = m[[x,y]][0]=~/[l]/
            weight += m[[x,y]][1][i].to_f
            x -= 1
          else
            tf = false
          end
        end
      end
    else
      tf = false
    end
  
    if tf == false
      weight = nil
    else
      return weight
    end
  end


#-------------------------------------------------------

def distance(file, depth)
  table = Hash.new
  line = file.gets
  if line == nil then return end

  size, sx, sy, ex, ey = line.split(/\s/)
  size = Integer(size)

  while line = file.gets do
    if line[0...4] != "path"

      x, y, ds, w = line.split(/\s/,4)
      table["#{x} #{y}"] = "#{ds} #{w}"

    end
  end

  
  queue = []
  distance = []
  visited = []
  queue << "#{sx} #{sy}"
  visited << "#{sx} #{sy}"
  next_step = 0
  temp_step = 0
  depth["#{sx} #{sy}"] = 0

 while queue.any?
    current_node = queue.shift 
    x, y = current_node.split(/\s/)
    x, y = Integer(x), Integer(y)
    current = [x, y]
    adjacent = []
      ds, w = table ["#{x} #{y}"].split(/\s/, 2)
      ds.each_char { |d|
       adjacent.push("#{x + ((d == "l") ? -1 : ((d == "r") ? 1 : 0))} #{y + ((d == "u") ? -1 : ((d == "d") ? 1 : 0))}")

       }

    
       adjacent.each { |adjacent_cell|
        next if visited.include?(adjacent_cell)
        queue << adjacent_cell
        visited << adjacent_cell
        depth[adjacent_cell] = depth[current_node] + 1
    }
     
  end
    coordinates_dist = depth.sort
    coordinates_dist.each { |pair|
      x,y = pair[0].split
      coordinates_depth = Integer(pair[1])

      distance[coordinates_depth] = distance[coordinates_depth] == nil ? "" : distance[coordinates_depth]
      distance[coordinates_depth] += ((distance[coordinates_depth].length == 0 ? "#{coordinates_depth}," : ",") + "(#{x},#{y})")
    }
  
  return distance.join("\n")
end



#-------------------------------------------------------

def solve(file, depth)
  m = distance(file, depth)
  file.rewind
  table = Hash.new
  line = file.gets
  if line == nil then return end

  size, sx, sy, ex, ey = line.split(/\s/)
  size = Integer(size)

  while line = file.gets do
    if line[0...4] != "path"

      x, y, ds, w = line.split(/\s/,4)
      table["#{x} #{y}"] = "#{ds} #{w}"

    end
  end
  
  m.include?("(#{ex.to_i},#{ey.to_i})")
end

#-------------------------------------------------------

def main(command_name, file_name)
  maze_file = open(file_name)


  case command_name
  when "bridge"
    bridge(maze_file)
  when "print"
    pretty_print(maze_file)
  when "sortcells"
    sortcells(maze_file)
  when "open"
    open_cell(maze_file)
  when "paths"
    path(maze_file)
  when "distance"
    depth = Hash.new
    distance(maze_file, depth)
  when "solve" 
    depth = Hash.new
    solve(maze_file, depth)
  else
    fail "Invalid command"
  end
end
