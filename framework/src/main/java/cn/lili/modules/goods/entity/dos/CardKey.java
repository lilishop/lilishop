package cn.lili.modules.goods.entity.dos;

import cn.lili.modules.goods.entity.enums.CardKeyStatusEnum;
import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * 电子卡券卡密池实体，对应表 {@code li_card_key}。
 * <p>
 * 可售库存 = 同 SKU 下 status=UNUSED 且 delete_flag=0 的行数；card_secret 库内明文存储。
 *
 * @author Mike
 * @date 2026-07-31
 */
@Data
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor
@TableName("li_card_key")
@Schema(description = "电子卡券卡密")
public class CardKey extends BaseEntity {

    private static final long serialVersionUID = 1L;

    @Schema(description = "SKU ID")
    private String skuId;

    @Schema(description = "商品 SPU ID")
    private String goodsId;

    @Schema(description = "店铺 ID")
    private String storeId;

    @Schema(description = "卡号")
    private String cardNo;

    @Schema(description = "卡密(明文)")
    private String cardSecret;

    /**
     * @see CardKeyStatusEnum
     */
    @Schema(description = "状态 UNUSED/ALLOCATED/VOIDED")
    private String status;

    @Schema(description = "主订单号")
    private String orderSn;

    @Schema(description = "订单项号")
    private String orderItemSn;

    @Schema(description = "买家 ID")
    private String memberId;

    @Schema(description = "发卡时间")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private Date allocatedTime;
}
