package cn.lili.modules.member.entity.dto;

import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.utils.StringUtils;
import cn.lili.common.vo.PageVO;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * 评价查询条件
 *
 * @author Bulbasaur
 * @date 2020/11/30 14:52
 */
@Data
public class EvaluationQueryParams extends PageVO {


    @ApiModelProperty(value = "买家ID")
    private String memberId;

    @ApiModelProperty(value = "会员名称")
    private String memberName;

    @ApiModelProperty(value = "卖家名称")
    private String storeName;

    @ApiModelProperty(value = "卖家ID")
    private String storeId;

    @ApiModelProperty(value = "商品名称")
    private String goodsName;

    @ApiModelProperty(value = "商品ID")
    private String goodsId;

    @ApiModelProperty(value = "好中差评 , GOOD：好评，MODERATE：中评，WORSE：差评", allowableValues = "GOOD,MODERATE,WORSE")
    private String grade;

    @ApiModelProperty(value = "是否有图")
    private String haveImage;

    @ApiModelProperty(value = "评论日期--开始时间")
    private String startTime;

    @ApiModelProperty(value = "评论日期--结束时间")
    private String endTime;

    @ApiModelProperty(value = "状态")
    private String status;

    public EvaluationQueryParams() {

    }

    public <T> QueryWrapper<T> queryWrapper() {
        QueryWrapper<T> queryWrapper = new QueryWrapper<>();
        if (StringUtils.isNotEmpty(startTime) && StringUtils.isNotEmpty(endTime)) {
            queryWrapper.between("create_time", startTime, endTime);
        }
        if (StringUtils.isNotEmpty(grade)) {
            queryWrapper.eq("grade", grade);
        }
        if (StringUtils.isNotEmpty(goodsName)) {
            queryWrapper.like("goods_name", goodsName);
        }
        if (StringUtils.isNotEmpty(storeName)) {
            queryWrapper.like("store_name", storeName);
        }
        if (StringUtils.isNotEmpty(memberName)) {
            queryWrapper.like("member_name", memberName);
        }
        if (StringUtils.isNotEmpty(goodsId)) {
            queryWrapper.eq("goods_id", goodsId);
        }
        if (StringUtils.isNotEmpty(storeId)) {
            queryWrapper.eq("store_id", storeId);
        }
        if (StringUtils.isNotEmpty(memberId)) {
            queryWrapper.eq("member_id", memberId);
        }
        if (StringUtils.isNotEmpty(haveImage)) {
            queryWrapper.eq("have_image", haveImage);
        }
        if (StringUtils.isNotEmpty(status)) {
            queryWrapper.eq("status", status);
        }
        queryWrapper.eq("delete_flag", false);
        queryWrapper.orderByDesc("create_time");
        return queryWrapper;
    }
}
