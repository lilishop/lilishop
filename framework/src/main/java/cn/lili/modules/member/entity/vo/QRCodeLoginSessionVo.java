package cn.lili.modules.member.entity.vo;

import cn.lili.modules.member.entity.enums.QRCodeLoginSessionStatusEnum;
import lombok.Data;

import java.io.Serializable;

@Data
public class QRCodeLoginSessionVo implements Serializable {


    private static final long serialVersionUID = 8793639296995408322L;

    private String token;

    private Integer status;

    private long duration;

    private long userId;
}
