package cn.lili.mybatis.sharding;

import cn.lili.common.utils.DateUtil;
import com.google.common.collect.Range;
import org.apache.shardingsphere.api.sharding.standard.PreciseShardingAlgorithm;
import org.apache.shardingsphere.api.sharding.standard.PreciseShardingValue;
import org.apache.shardingsphere.api.sharding.standard.RangeShardingAlgorithm;
import org.apache.shardingsphere.api.sharding.standard.RangeShardingValue;

import java.util.ArrayList;
import java.util.Collection;

/**
 * 按创建时间年份分库
 *
 * @author Chopper
 */
public class CreateTimeShardingDatabaseAlgorithm implements PreciseShardingAlgorithm<Long>, RangeShardingAlgorithm {


    @Override
    public String doSharding(Collection<String> collection, PreciseShardingValue<Long> preciseShardingValue) {
        Long createTime = preciseShardingValue.getValue();
        String value = DateUtil.toString(createTime, "yyyy");
        //data2019,data2020
        return "data" + value;
    }

    @Override
    public Collection<String> doSharding(Collection collection, RangeShardingValue rangeShardingValue) {
        Collection<String> collect = new ArrayList<>();
        Range<Integer> valueRange = rangeShardingValue.getValueRange();

        //开始年份结束年份
        String start = DateUtil.toString(valueRange.lowerEndpoint().longValue(), "yyyy");
        String end = DateUtil.toString(valueRange.upperEndpoint().longValue(), "yyyy");
        //循环增加区间的查询条件
        for (Integer i = Integer.valueOf(start); i <= Integer.valueOf(end); i++) {
            collect.add("data" + i);
        }
        return collect;
    }
}

