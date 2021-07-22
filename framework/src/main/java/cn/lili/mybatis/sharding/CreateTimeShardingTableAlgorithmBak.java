package cn.lili.mybatis.sharding;

import cn.lili.common.utils.DateUtil;
import org.apache.shardingsphere.api.sharding.standard.PreciseShardingAlgorithm;
import org.apache.shardingsphere.api.sharding.standard.PreciseShardingValue;
import org.apache.shardingsphere.api.sharding.standard.RangeShardingAlgorithm;
import org.apache.shardingsphere.api.sharding.standard.RangeShardingValue;

import java.util.ArrayList;
import java.util.Collection;

/**
 * 按创建时间月份分表
 *
 * @author Chopper
 */
public class CreateTimeShardingTableAlgorithmBak implements PreciseShardingAlgorithm<Long>, RangeShardingAlgorithm {


    @Override
    public String doSharding(Collection<String> collection, PreciseShardingValue<Long> preciseShardingValue) {
        Long createTime = preciseShardingValue.getValue();
        String value = DateUtil.toString(createTime, "MM");
        Integer month = Integer.valueOf(value);
        //li_order_1,li_order_2~
        return "li_order_" + month;

    }

    @Override
    public Collection<String> doSharding(Collection collection, RangeShardingValue rangeShardingValue) {
        Collection<String> collect = new ArrayList<>();
        //因为考虑到 假设2019-05～2020-05
        //这快是没办法处理的，因为分库分表之后，每个库都要进行查询，如果操作为，1-4月，那么2020年数据查询正确了，可是2019年到5-12月数据就查询不到了
        //这里需要做一些性能的浪费，现在看来是没办法处理到
        for (Integer i = 1; i <= 12; i++) {
            collect.add("li_order_" + i);
        }
        return collect;
    }
}

