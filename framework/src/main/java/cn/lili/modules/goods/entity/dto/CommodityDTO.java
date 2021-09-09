package cn.lili.modules.goods.entity.dto;

import lombok.Data;

/**
 * 直播商品DTO
 * 用于获取直播商品状态时使用
 *
 * @author Bulbasaur
 * @since 2021/5/25 12:12 下午
 */
@Data
public class CommodityDTO {
    /**
     * 商品ID
     */
    private Integer goods_id;
    /**
     * 商品名称
     */
    private String name;
    /**
     * 地址
     */
    private String url;
    /**
     * 审核状态
     */
    private Integer audit_status;
}
