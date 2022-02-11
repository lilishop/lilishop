-- 可能回滚的列表，一个记录要回滚的skuid一个记录库存
local id_list= {}
local quantity_list= {}

-- 调用放传递的keys 和 values  execute(RedisScript<T> script, List<K> keys, Object... args)
local keys = KEYS
local values = ARGV;

local function deduction(key,num)
    keys[1] = key;
    local value = redis.call("get",keys[1])
    if not value then
        value = 0;
    end
    value = value + num
    -- 变更后库存数量小于
    if(value<0)
    then
        -- 发生超卖
        return false;
    end
    redis.call("set",keys[1],value)

    return true
end

local function rollback()
    for i,k in ipairs (id_list) do
        -- 还原库存
        keys[1] = k;
        redis.call("incrby",keys[1],0-quantity_list[i])
    end
end

local function execute()
    -- i 类java for循环 for(int i=0;i<?;i++) 下标
    -- k 为遍历的值 具体值，非下标
    for i, k in ipairs (values)
    do
        -- num 变更数量
        -- key 为缓存key
        local num = tonumber(k)
        local key=  keys[i]
        -- 进行库存扣减，为false 代表扣减失败，要进行回滚
        local result = deduction(key,num)

        -- 回滚
        if (result == false)
        then
            rollback()
            return false
        else
            -- 记录可能要回滚的数据
            table.insert(id_list,key)
            table.insert(quantity_list,num)
        end

    end
    return true;
end

return execute()