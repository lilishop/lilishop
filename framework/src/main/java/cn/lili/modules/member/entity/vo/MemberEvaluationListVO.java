package cn.lili.modules.member.entity.vo;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * 会员评价VO
 *
 * @author Bulbasaur
 * @since 2020/11/30 15:00
 */
@Data
public class MemberEvaluationListVO {

    @ApiModelProperty(value = "评论ID")
    private String id;

    @ApiModelProperty(value = "会员名称")
    private String memberName;

    @ApiModelProperty(value = "商品名称")
    private String goodsName;

    @ApiModelProperty(value = "好中差评", allowableValues = "GOOD,NEUTRAL,BAD")
    private String grade;

    @ApiModelProperty(value = "评价内容")
    private String content;

    @ApiModelProperty(value = "状态 ", allowableValues = " OPEN 正常 ,CLOSE 关闭")
    private String status;

    @ApiModelProperty(value = "回复状态")
    private Boolean replyStatus;

    @ApiModelProperty(value = "创建时间")
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss", timezone = "GMT+8")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private Date createTime;

    @ApiModelProperty(value = "物流评分")
    private Integer deliveryScore;

    @ApiModelProperty(value = "服务评分")
    private Integer serviceScore;

    @ApiModelProperty(value = "描述评分")
    private Integer descriptionScore;
}
