package cn.lili.modules.member.entity.dto;

import lombok.AllArgsConstructor;
import lombok.Data;

/**
 * 收藏数量变化DTO
 *
 * @author Chopper
 * @version v1.0
 * 2021-11-30 10:14
 */
@Data
@AllArgsConstructor
public class CollectionDTO {

    /**
     * 变化的模型id
     * 商品id/店铺id
     */
    private String id;

    /**
     * 变化的数量
     * -1 减少1 / +1 增加1
     */
    private Integer num;
}
