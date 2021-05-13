package cn.lili.modules.base.entity.vo;

import lombok.AllArgsConstructor;
import lombok.Data;

/**
 * REDIS VO
 * @author Chopper
 * @date 2020/12/2 17:50
 */
@Data
@AllArgsConstructor
public class RedisVo {

    private String key;

    private String value;
}
