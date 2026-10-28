package cn.lili.modules.member.entity.dto;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.vo.PageVO;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;

/**
 * 评价查询条件
 *
 * @author Bulbasaur
 * @since 2020/11/30 14:52
 */
@EqualsAndHashCode(callSuper = true)
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class EvaluationQueryParams extends PageVO {

    private static final long serialVersionUID = 2038158669175297129L;

    @Schema(description = "ID")
    private String id;

    @Schema(description = "买家ID")
    private String memberId;

    @Schema(description = "skuID")
    private String skuId;

    @Schema(description = "客户名称")
    private String memberName;

    @Schema(description = "卖家名称")
    private String storeName;

    @Schema(description = "卖家ID")
    private String storeId;

    @Schema(description = "商品名称")
    private String goodsName;

    @Schema(description = "商品ID")
    private String goodsId;

    @Schema(description = "好中差评 , GOOD：好评，MODERATE：中评，WORSE：差评", allowableValues = {"GOOD","MODERATE","WORSE"})
    private String grade;

    @Schema(description = "是否有图")
    private String haveImage;

    @Schema(description = "回复状态")
    private Boolean replyStatus;

    @Schema(description = "评论日期--开始时间")
    private String startTime;

    @Schema(description = "评论日期--结束时间")
    private String endTime;

    @Schema(description = "状态")
    private String status;

    public <T> QueryWrapper<T> queryWrapper() {
        QueryWrapper<T> queryWrapper = new QueryWrapper<>();
        if (CharSequenceUtil.isNotEmpty(id)) {
            queryWrapper.eq("id", id);
        }
        if (CharSequenceUtil.isNotEmpty(startTime) && CharSequenceUtil.isNotEmpty(endTime)) {
            queryWrapper.between("create_time", startTime, endTime);
        }
        if (CharSequenceUtil.isNotEmpty(grade)) {
            queryWrapper.eq("grade", grade);
        }
        if (CharSequenceUtil.isNotEmpty(goodsName)) {
            queryWrapper.like("goods_name", goodsName);
        }
        if (CharSequenceUtil.isNotEmpty(storeName)) {
            queryWrapper.like("store_name", storeName);
        }
        if (CharSequenceUtil.isNotEmpty(memberName)) {
            queryWrapper.like("member_name", memberName);
        }
        if (CharSequenceUtil.isNotEmpty(goodsId)) {
            queryWrapper.eq("goods_id", goodsId);
        }
        if (CharSequenceUtil.isNotEmpty(skuId)) {
            queryWrapper.eq("sku_id", skuId);
        }
        if (CharSequenceUtil.isNotEmpty(storeId)) {
            queryWrapper.eq("store_id", storeId);
        }
        if (CharSequenceUtil.isNotEmpty(memberId)) {
            queryWrapper.eq("member_id", memberId);
        }
        if (CharSequenceUtil.isNotEmpty(haveImage)) {
            queryWrapper.eq("have_image", haveImage);
        }
        if (replyStatus != null) {
            queryWrapper.eq("reply_status", replyStatus);
        }
        if (CharSequenceUtil.isNotEmpty(status)) {
            queryWrapper.eq("status", status);
        }
        queryWrapper.eq("delete_flag", false);
        queryWrapper.orderByDesc("top");
        queryWrapper.orderByDesc("create_time");
        return queryWrapper;
    }
}
