package cn.lili.modules.member.entity.dto;

import cn.hutool.core.date.DateUtil;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.utils.StringUtils;
import cn.lili.common.vo.PageVO;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * 店铺评价查询参数
 *
 * @author Chopper
 * @date 2021/3/20 10:43
 */

@Data
public class StoreEvaluationQueryParams extends PageVO {

    @ApiModelProperty(value = "会员名称")
    private String memberName;

    @ApiModelProperty(value = "商品名称")
    private String goodsName;

    @ApiModelProperty(value = "好中差评 good：好评，neutral：中评，bad：差评", allowableValues = "GOOD,NEUTRAL,BAD")
    private String grade;

    @ApiModelProperty(value = "评论日期--开始时间")
    private String startDate;

    @ApiModelProperty(value = "评论日期--结束时间")
    private String endDate;


    public <T> QueryWrapper<T> queryWrapper() {
        QueryWrapper<T> queryWrapper = new QueryWrapper<>();

        queryWrapper.eq("store_id", UserContext.getCurrentUser().getStoreId());

        if (StringUtils.isNotEmpty(startDate) && StringUtils.isNotEmpty(endDate)) {
            queryWrapper.between("create_time", DateUtil.parse(startDate), DateUtil.parse(endDate));
        }

        if (StringUtils.isNotEmpty(grade)) {
            queryWrapper.eq("grade", grade);
        }

        if (StringUtils.isNotEmpty(goodsName)) {
            queryWrapper.eq("goods_name", goodsName);
        }

        if (StringUtils.isNotEmpty(memberName)) {
            queryWrapper.eq("member_name", memberName);
        }
        return queryWrapper;
    }
}
