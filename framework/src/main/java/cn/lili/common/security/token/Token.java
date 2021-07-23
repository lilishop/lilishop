package cn.lili.common.security.token;

import lombok.Data;

/**
 * Token 实体类
 *
 * @author Chopper
 * @version v1.0
 * 2020-11-13 10:02
 */
@Data
public class Token {
    /**
     * 访问token
     */
    private String accessToken;

    /**
     * 刷新token
     */
    private String refreshToken;

}
