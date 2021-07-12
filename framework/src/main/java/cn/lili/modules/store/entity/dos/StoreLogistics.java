package cn.lili.modules.store.entity.dos;

import cn.lili.base.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.Entity;
import javax.persistence.Table;
import javax.validation.constraints.NotNull;

/**
 * 店铺-物流公司设置
 *
 * @author Chopper
 * @date 2020/11/17 8:01 下午
 */
@Data
@Entity
@Table(name = "li_store_logistics")
@TableName("li_store_logistics")
@ApiModel(value = "店铺-物流公司")
@AllArgsConstructor
@NoArgsConstructor
public class StoreLogistics extends BaseEntity {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty(value = "店铺ID")
    private String storeId;

    @ApiModelProperty(value = "物流公司ID")
    @NotNull
    private String logisticsId;


}