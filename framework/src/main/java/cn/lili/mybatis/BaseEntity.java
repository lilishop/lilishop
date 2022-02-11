package cn.lili.mybatis;

import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.CreatedBy;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.elasticsearch.annotations.DateFormat;
import org.springframework.data.elasticsearch.annotations.Field;
import org.springframework.data.elasticsearch.annotations.FieldType;
import org.springframework.format.annotation.DateTimeFormat;

import java.io.Serializable;
import java.util.Date;


/**
 * 数据库基础实体类
 *
 * @author Chopper
 * @version v1.0
 * @since 2020/8/20 14:34
 */
@Data
@JsonIgnoreProperties(value = { "handler", "fieldHandler"})
@AllArgsConstructor
@NoArgsConstructor
public abstract class BaseEntity implements Serializable {

    private static final long serialVersionUID = 1L;


    @TableId
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
    @Field(type = FieldType.Date, format = DateFormat.custom, pattern = "yyyy-MM-dd HH:mm:ss || yyyy-MM-dd || yyyy/MM/dd HH:mm:ss|| yyyy/MM/dd ||epoch_millis")
    private Date createTime;

    @LastModifiedBy
    @TableField(fill = FieldFill.UPDATE)
    @ApiModelProperty(value = "更新者", hidden = true)
    private String updateBy;

    @LastModifiedDate
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @TableField(fill = FieldFill.UPDATE)
    @ApiModelProperty(value = "更新时间", hidden = true)
    @Field(type = FieldType.Date, format = DateFormat.custom, pattern = "yyyy-MM-dd HH:mm:ss || yyyy-MM-dd || yyyy/MM/dd HH:mm:ss|| yyyy/MM/dd ||epoch_millis")
    private Date updateTime;

    @TableField(fill = FieldFill.INSERT)
    @ApiModelProperty(value = "删除标志 true/false 删除/未删除", hidden = true)
    private Boolean deleteFlag;

}
