package cn.lili.modules.goods.entity.dos;

import cn.lili.mybatis.BaseIdEntity;
import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import org.springframework.data.annotation.CreatedBy;

/**
 * 商品相册
 *
 * @author pikachu
 * @since 2020-02-23 9:14:33
 */
@Data
@TableName("li_goods_gallery")
@ApiModel(value = "商品相册")
public class GoodsGallery extends BaseIdEntity {


    @CreatedBy
    @TableField(fill = FieldFill.INSERT)
    @ApiModelProperty(value = "创建者", hidden = true)
    private String createBy;

    /**
     * 商品主键
     */
    @ApiModelProperty(value = "商品id")
    private String goodsId;

    /**
     * 缩略图路径
     */
    @ApiModelProperty(value = "缩略图路径")
    private String thumbnail;

    /**
     * 小图路径
     */
    @ApiModelProperty(value = "小图路径")
    private String small;

    /**
     * 原图路径
     */
    @ApiModelProperty(value = "原图路径", required = true)
    private String original;

    /**
     * 是否是默认图片1   0没有默认
     */
    @ApiModelProperty(value = "是否是默认图片1   0没有默认")
    private Integer isDefault;

    /**
     * 排序
     */
    @ApiModelProperty(value = "排序", required = true)
    private Integer sort;

}