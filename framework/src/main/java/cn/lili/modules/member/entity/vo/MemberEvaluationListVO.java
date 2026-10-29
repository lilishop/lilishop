package cn.lili.modules.member.entity.vo;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * 客户评价VO
 *
 * @author Bulbasaur
 * @since 2020/11/30 15:00
 */
@Data
public class MemberEvaluationListVO {

    @Schema(description = "评论ID")
    private String id;

    @Schema(description = "客户ID")
    private String memberId;

    @Schema(description = "店铺ID")
    private String storeId;

    @Schema(description = "店铺名称")
    private String storeName;

    @Schema(description = "评价图片")
    private String images;

    @Schema(description = "客户名称")
    private String memberName;

    @Schema(description = "客户头像")
    private String memberProfile;

    @Schema(description = "商品名称")
    private String goodsName;

    @Schema(description = "商品图片")
    private String goodsImage;

    @Schema(description = "订单号")
    private String orderNo;

    @Schema(description = "商品ID")
    private String goodsId;

    @Schema(description = "好中差评", allowableValues = "GOOD,NEUTRAL,BAD")
    private String grade;

    @Schema(description = "评价内容")
    private String content;

    @Schema(description = "状态 ", allowableValues = " OPEN 正常 ,CLOSE 关闭")
    private String status;

    @Schema(description = "回复状态")
    private Boolean replyStatus;

    @Schema(description = "是否置顶")
    private Boolean top;

    @Schema(description = "创建时间")
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss", timezone = "GMT+8")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private Date createTime;

    @Schema(description = "物流评分")
    private Integer deliveryScore;

    @Schema(description = "服务评分")
    private Integer serviceScore;

    @Schema(description = "描述评分")
    private Integer descriptionScore;
}
