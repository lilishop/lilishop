package cn.lili.modules.member.entity.vo;

import cn.lili.common.security.token.Token;
import lombok.Data;

@Data
public class QRLoginResultVo {

    private Token token;

    private int status;
}
