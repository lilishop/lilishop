package cn.lili.mybatis.sharding;

import cn.hutool.core.convert.Convert;
import cn.lili.common.utils.DateUtil;
import com.google.common.collect.Range;
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
public class CreateTimeShardingTableAlgorithm implements PreciseShardingAlgorithm<Long>, RangeShardingAlgorithm {


    @Override
    public String doSharding(Collection<String> collection, PreciseShardingValue<Long> preciseShardingValue) {
        Long createTime = preciseShardingValue.getValue();
        String monthValue = DateUtil.toString(createTime, "MM");
        String yearValue = DateUtil.toString(createTime, "yyyy");
        Integer month = Integer.valueOf(monthValue);
        Integer year = Integer.valueOf(yearValue);
        //li_order_1,li_order_2~
        return "li_order_" + year + "_" + month;
    }

    @Override
    public Collection<String> doSharding(Collection collection, RangeShardingValue rangeShardingValue) {
        Collection<String> collect = new ArrayList<>();
        Range<Integer> valueRange = rangeShardingValue.getValueRange();

        Integer startMonth = Convert.toInt(DateUtil.toString(valueRange.lowerEndpoint().longValue(), "MM"));
        Integer endMonth = Convert.toInt(DateUtil.toString(valueRange.upperEndpoint().longValue(), "MM"));
        Integer startYear = Convert.toInt(DateUtil.toString(valueRange.lowerEndpoint().longValue(), "yyyy"));
        Integer endYear = Convert.toInt(DateUtil.toString(valueRange.upperEndpoint().longValue(), "yyyy"));

        //如果是同一年查询
        //2020-1~2020-2
        if (startYear.equals(endYear)) {
            for (Integer i = startYear; i <= endYear; i++) {
                for (Integer j = startMonth; j <= endMonth; j++) {
                    collect.add("li_order_" + i + "_" + j);
                }
            }
        }
        //2020-1~2021-2
        else {
            for (Integer i = startYear; i <= endYear; i++) {
                //如果是第一年
                if (i.equals(startYear)) {
                    //计算从 开始月份 到 今年到12月
                    for (Integer j = startMonth; j <= 12; j++) {
                        collect.add("li_order_" + i + "_" + j);
                    }
                }
                //如果是最后一年
                else if (i.equals(endYear)) {
                    //计算从 1月 到 最后一年结束月份
                    for (Integer j = 1; j <= endMonth; j++) {
                        collect.add("li_order_" + i + "_" + j);
                    }
                }
                //中间年份处理
                else {
                    //中间年份，每个月都要进行查询处理
                    for (Integer j = 1; j <= 12; j++) {
                        collect.add("li_order_" + i + "_" + j);
                    }
                }
            }
        }

        return collect;
    }
}

