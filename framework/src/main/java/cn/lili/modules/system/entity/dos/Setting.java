package cn.lili.modules.system.entity.dos;

import cn.lili.base.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.Entity;
import javax.persistence.Table;

/**
 * 设置
 * @author Chopper
 * @date 2020-02-25 14:10:16
 */
@Data
@Entity
@Table(name = "li_setting")
@TableName("li_setting")
@ApiModel(value = "配置")
@NoArgsConstructor
public class Setting extends BaseEntity {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty(value = "配置值value")
    private String settingValue;

}