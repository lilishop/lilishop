package cn.lili.modules.im.entity.vo;

import cn.lili.modules.im.entity.dos.ImTalk;
import cn.lili.mybatis.BaseTenantEntity;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * @author Chopper
 */
@Data
@ApiModel(value = "聊天")
public class ImTalkVO extends BaseTenantEntity {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("id")
    private String id;

    @ApiModelProperty("用户 id")
    private String userId;

    @ApiModelProperty("置顶")
    private Boolean top;

    @ApiModelProperty("用户 不可见")
    private Boolean disable;

    @ApiModelProperty("用户名字")
    private String name;

    @ApiModelProperty("用户头像")
    private String face;

    @ApiModelProperty("店铺标识")
    private Boolean storeFlag;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @ApiModelProperty(value = "最后聊天时间", hidden = true)
    private Date lastTalkTime;

    @ApiModelProperty(value = "最后聊天内容")
    private String lastTalkMessage;

    @ApiModelProperty(value = "最后发送消息类型")
    private String lastMessageType;

    @ApiModelProperty(value = "未读数量")
    private Long unread;

    public ImTalkVO() {

    }

    public ImTalkVO(ImTalk imTalk, String currentUser) {
        if (imTalk.getUserId2().equals(currentUser)) {
            userId = imTalk.getUserId1();
            top = imTalk.getTop1();
            disable = imTalk.getDisable1();
            name = imTalk.getName1();
            face = imTalk.getFace1();
            storeFlag = imTalk.getStoreFlag1();
        } else {
            userId = imTalk.getUserId2();
            top = imTalk.getTop2();
            disable = imTalk.getDisable2();
            name = imTalk.getName2();
            face = imTalk.getFace2();
            storeFlag = imTalk.getStoreFlag2();
        }
        lastTalkMessage = imTalk.getLastTalkMessage();
        lastTalkTime = imTalk.getLastTalkTime();
        lastMessageType = imTalk.getLastMessageType();
        id = imTalk.getId();
    }
}