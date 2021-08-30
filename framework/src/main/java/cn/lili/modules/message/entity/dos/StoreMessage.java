package cn.lili.modules.message.entity.dos;

import cn.lili.modules.message.entity.enums.MessageStatusEnum;
import cn.lili.mybatis.BaseIdEntity;
import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import org.springframework.data.annotation.CreatedBy;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * 店铺接收消息对象
 *
 * @author Chopper
 * @since 2021/1/30 4:13 下午
 */
@Data
@TableName("li_store_message")
@ApiModel(value = "店铺消息")
public class StoreMessage extends BaseIdEntity {

    private static final long serialVersionUID = 1L;

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

    @ApiModelProperty(value = "关联消息id")
    private String messageId;

    @ApiModelProperty(value = "关联店铺id")
    private String storeId;

    @ApiModelProperty(value = "关联店铺名称")
    private String storeName;

    /**
     * @see MessageStatusEnum
     */
    @ApiModelProperty(value = "状态 0默认未读 1已读 2回收站")
    private String status = MessageStatusEnum.UN_READY.name();


    @TableField(exist = false)
    @ApiModelProperty(value = "消息标题")
    private String title;

    @TableField(exist = false)
    @ApiModelProperty(value = "消息内容")
    private String content;
}