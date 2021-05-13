package cn.lili.modules.goods.entity.vos;

import com.baomidou.mybatisplus.annotation.TableField;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.util.List;

/**
 * 规格值VO
 *
 * @author paulG
 * @date 2020-02-26 23:24:13
 */
@Data
public class GoodsSpecValueVO {

    /**
     * 规格值名字
     */
    @TableField(value = "name")
    @ApiModelProperty(value = "规格值名字")
    private String name;

    /**
     * 规格值名字
     */
    @TableField(value = "value")
    @ApiModelProperty(value = "规格值")
    private List<String> value;

}
