package cn.lili.modules.promotion.entity.dto.search;

import cn.hutool.core.text.CharSequenceUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;


/**
 * 砍价活动参与实体类
 *
 * @author qiuqiu
 * @date 2020-7-1 10:44 上午
 */
@Data
@ApiModel(value = "砍价活动参与记录查询对象")
public class KanJiaActivityLogQuery {


    private static final long serialVersionUID = -1583030890805926292L;

    @ApiModelProperty(value = "砍价发起活动id")
    private String kanJiaActivityId;


    public <T> QueryWrapper<T> wrapper() {
        QueryWrapper<T> queryWrapper = new QueryWrapper<>();

        if (CharSequenceUtil.isNotEmpty(kanJiaActivityId)) {
            queryWrapper.like("kanjia_activity_id", kanJiaActivityId);
        }
        queryWrapper.eq("delete_flag", false);
        queryWrapper.orderByDesc("create_time");
        return queryWrapper;
    }
}