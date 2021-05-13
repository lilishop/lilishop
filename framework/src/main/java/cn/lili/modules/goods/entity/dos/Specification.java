package cn.lili.modules.goods.entity.dos;

import cn.lili.base.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.persistence.Entity;
import javax.persistence.Table;
import javax.validation.constraints.NotEmpty;

/**
 * 商品规格项
 *
 * @author pikachu
 * @date 2020-02-18 15:18:56
 */
@Data
@Entity
@Table(name = "li_specification")
@TableName("li_specification")
@ApiModel(value = "规格项")
public class Specification extends BaseEntity {

    private static final long serialVersionUID = 147792597901239486L;

    /**
     * 规格名称
     */
    @NotEmpty(message = "规格名称不能为空")
    @ApiModelProperty(value = "规格名称", required = true)
    private String specName;

    /**
     * 所属卖家 0属于平台
     */
    @ApiModelProperty(hidden = true)
    private String storeId;


}