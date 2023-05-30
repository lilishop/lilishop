package cn.lili.modules.goods.entity.vos;

import com.baomidou.mybatisplus.annotation.TableField;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.io.Serializable;
import java.util.List;

/**
 * 规格值
 *
 * @author Chopper
 * @since 2020-02-26 23:24:13
 */
@Data
public class SpecValueVO implements Serializable {

    private static final long serialVersionUID = -4433579132929428572L;

    @TableField(value = "spec_name")
    @ApiModelProperty(value = "规格项名字")
    private String specName;

    @TableField(value = "spec_value")
    @ApiModelProperty(value = "规格值")
    private String specValue;

    @ApiModelProperty(value = "该规格是否有图片，1 有 0 没有")
    private Integer specType;
    /**
     * 规格图片
     */
    @ApiModelProperty(value = "规格的图片")
    private List<SpecImages> specImage;

    @Data
    public static class SpecImages implements Serializable {

        private static final long serialVersionUID = 1816357809660916086L;

        private String url;

        private String name;

        private String status;

    }
}
