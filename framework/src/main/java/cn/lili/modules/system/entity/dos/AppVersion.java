package cn.lili.modules.system.entity.dos;

import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import org.springframework.data.annotation.CreatedBy;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.format.annotation.DateTimeFormat;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import java.util.Date;


/**
 * app历史版本维护
 *
 * @author zh
 * @date 2020-06-20 09:29:19
 */
@Data
@Entity
@Table(name = "li_app_version")
@TableName("li_app_version")
@ApiModel(value = "app版本控制")
public class AppVersion {

    private static final long serialVersionUID = 3034686331756935L;

    @Id
    @TableId
    @TableField
    @Column(columnDefinition = "bigint(20)")
    @ApiModelProperty(value = "唯一标识", hidden = true)
    private String id;

    @CreatedDate
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @TableField(fill = FieldFill.INSERT)
    @ApiModelProperty(value = "创建时间", hidden = true)
    private Date createTime;

    @CreatedBy
    @TableField(fill = FieldFill.INSERT)
    @ApiModelProperty(value = "创建者", hidden = true)
    private String createBy;

    @ApiModelProperty(value = "版本号")
    private String version;

    @ApiModelProperty(value = "版本名称")
    private String versionName;

    @ApiModelProperty(value = "更新内容")
    private String content;

    @ApiModelProperty(value = "是否强制更新")
    private Boolean forceUpdate;

    @ApiModelProperty(value = "下载地址")
    private String downloadUrl;

    /**
     * @see cn.lili.modules.system.entity.enums.AppType
     */
    @ApiModelProperty(value = "类型")
    private String type;

    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @ApiModelProperty(value = "版本更新时间")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date versionUpdateDate;

}