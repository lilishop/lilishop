package cn.lili.modules.permission.entity.dos;

import cn.lili.base.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.Entity;
import javax.persistence.Table;

/**
 * 角色部门绑定关系
 *
 * @author Chopper
 * @date 2020/11/19 12:18
 */
@Data
@Entity
@Table(name = "li_department_role")
@TableName("li_department_role")
@ApiModel(value = "角色部门")
@NoArgsConstructor
@AllArgsConstructor
public class DepartmentRole extends BaseEntity {


    private static final long serialVersionUID = 2342812932116647050L;

    @ApiModelProperty(value = "角色id")
    private String roleId;

    @ApiModelProperty(value = "部门id")
    private String departmentId;

}