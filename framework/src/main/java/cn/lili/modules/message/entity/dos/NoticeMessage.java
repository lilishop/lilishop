package cn.lili.modules.message.entity.dos;

import cn.lili.base.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.persistence.Entity;
import javax.persistence.Table;

/**
 * 通知类站内信模版对象
 *
 * @author Bulbasaur
 * @version v4.1
 * @date 2020/12/8 9:46
 */
@Data
@Entity
@Table(name = "li_notice_message")
@TableName("li_notice_message")
@ApiModel(value = "通知类消息模板")
public class NoticeMessage extends BaseEntity {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty(value = "站内信节点")
    private String noticeNode;

    @ApiModelProperty(value = "站内信标题")
    private String noticeTitle;

    @ApiModelProperty(value = "站内信内容")
    private String noticeContent;
    /**
     * @see cn.lili.common.enums.SwitchEnum
     */
    @ApiModelProperty(value = "站内信是否开启")
    private String noticeStatus;
    /**
     * @see cn.lili.modules.message.entity.enums.NoticeMessageParameterEnum
     */
    @ApiModelProperty(value = "消息变量")
    private String variable;


}