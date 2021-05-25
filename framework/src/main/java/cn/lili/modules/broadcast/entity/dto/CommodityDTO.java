package cn.lili.modules.broadcast.entity.dto;

import lombok.Data;

/**
 * 直播商品DTO
 * 用于获取直播商品状态时使用
 *
 * @author Bulbasaur
 * @date: 2021/5/25 12:12 下午
 */
@Data
public class CommodityDTO {

    private Integer goods_id;
    private String name;
    private String url;
    private Integer audit_status;
}
