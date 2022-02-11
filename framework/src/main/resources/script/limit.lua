local c
c = redis.call('get',KEYS[1])
-- 调用不超过最大值，则直接返回
if c and tonumber(c) > tonumber(ARGV[1]) then
    return c;
end
-- 执行计算器自加
c = redis.call('incr',KEYS[1])
if tonumber(c) == 1 then
-- 从第一次调用开始限流，设置对应键值的过期
    redis.call('expire',KEYS[1],ARGV[2])
end
return c;