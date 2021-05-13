package cn.lili.modules.connect.entity;

import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.CreatedBy;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.format.annotation.DateTimeFormat;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import java.io.Serializable;
import java.util.Date;

/**
 * @author Chopper
 */
@Data
@Entity
@Table(name = "li_connect")
@TableName("li_connect")
@ApiModel(value = "联合登陆")
@NoArgsConstructor
public class Connect implements Serializable {

    private static final long serialVersionUID = 1L;

    @Id
    @TableId
    @TableField
    @Column(columnDefinition = "bigint(20)")
    @ApiModelProperty(value = "唯一标识", hidden = true)
    private String id;


    @CreatedBy
    @TableField(fill = FieldFill.INSERT)
    @ApiModelProperty(value = "创建者", hidden = true)
    private String createBy;

    @CreatedDate
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @TableField(fill = FieldFill.INSERT)
    @ApiModelProperty(value = "创建时间", hidden = true)
    private Date createTime;

    @ApiModelProperty("用户id")
    private String userId;

    @ApiModelProperty("联合登录id")
    private String unionId;

    /**
     * @see cn.lili.modules.connect.entity.enums.ConnectEnum
     */
    @ApiModelProperty(value = "联合登录类型")
    private String unionType;


    public Connect(String userId, String unionId, String unionType) {
        this.userId = userId;
        this.unionId = unionId;
        this.unionType = unionType;
    }
}