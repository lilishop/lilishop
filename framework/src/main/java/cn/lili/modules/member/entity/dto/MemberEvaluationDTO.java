package cn.lili.modules.member.entity.dto;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import org.hibernate.validator.constraints.Length;

import javax.validation.constraints.NotEmpty;

/**
 * 会员评价DTO
 *
 * @author Chopper
 * @since 2020/11/29 11:13 下午
 */
@Data
public class MemberEvaluationDTO {

    @ApiModelProperty(value = "子订单编号")
    @NotEmpty(message = "订单异常")
    private String orderItemSn;

    @ApiModelProperty(value = "商品ID")
    @NotEmpty(message = "订单商品异常不能为空")
    private String goodsId;

    @ApiModelProperty(value = "规格ID")
    @NotEmpty(message = "订单商品不能为空")
    private String skuId;

    @ApiModelProperty(value = "好中差评价")
    @NotEmpty(message = "请评价")
    private String grade;

    @ApiModelProperty(value = "评论内容")
    @NotEmpty(message = "评论内容不能为空")
    @Length(max = 500, message = "评论内容不能超过500字符")
    private String content;

    @ApiModelProperty(value = "评论图片")
    private String images;

    @ApiModelProperty(value = "物流评分")
    private Integer deliveryScore;

    @ApiModelProperty(value = "服务评分")
    private Integer serviceScore;

    @ApiModelProperty(value = "描述评分")
    private Integer descriptionScore;


}
