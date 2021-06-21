package cn.lili.modules.broadcast.entity.dto;

import io.swagger.annotations.ApiModelProperty;

/**
 * 用于直播间前台使用的直播间商品DTO
 *
 * @author Bulbasaur
 * @date: 2021/5/20 2:34 下午
 */
public class SimpleCommodity {

    @ApiModelProperty(value = "图片")
    private String goodsImage;

    @ApiModelProperty(value = "商品名称")
    private String name;
}
