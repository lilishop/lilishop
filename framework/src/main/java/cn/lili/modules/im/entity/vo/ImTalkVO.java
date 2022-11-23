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

    /**
     * id
     */
    private String id;
    /**
     * 用户 id
     */
    private String userId;

    /**
     * 置顶
     */
    private Boolean top;

    /**
     * 用户 不可见
     */
    private Boolean disable;

    /**
     * 用户名字
     */
    private String name;

    /**
     * 用户头像
     */
    private String face;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @ApiModelProperty(value = "最后聊天时间", hidden = true)
    private Date lastTalkTime;

    public ImTalkVO() {

    }

    public ImTalkVO(ImTalk imTalk, String currentUser) {
        if (imTalk.getUserId2().equals(currentUser)) {
            userId = imTalk.getUserId1();
            top = imTalk.getTop1();
            disable = imTalk.getDisable1();
            name = imTalk.getName1();
            face = imTalk.getFace1();
        } else {
            userId = imTalk.getUserId2();
            top = imTalk.getTop2();
            disable = imTalk.getDisable2();
            name = imTalk.getName2();
            face = imTalk.getFace2();
        }

        lastTalkTime = imTalk.getLastTalkTime();
        id = imTalk.getId();
    }
}