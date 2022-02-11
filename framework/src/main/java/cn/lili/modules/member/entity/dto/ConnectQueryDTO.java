package cn.lili.modules.member.entity.dto;

import lombok.Builder;
import lombok.Data;

/**
 * ConnectQueryDTO
 *
 * @author Chopper
 * @version v1.0
 * 2021-12-01 14:34
 */
@Data
@Builder
public class ConnectQueryDTO {

    /**
     * 用户id
     */
    private String userId;

    /**
     * 第三方id
     */
    private String unionId;

    /**
     * 联合登陆类型
     */
    private String unionType;

}
