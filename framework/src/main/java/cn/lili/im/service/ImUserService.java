package cn.lili.im.service;

import cn.lili.im.entity.ImUser;
import com.baomidou.mybatisplus.extension.service.IService;

/**
 * Im消息 业务层
 *
 * @author Chopper
 */
public interface ImUserService extends IService<ImUser> {

    /**
     * 注册用户
     *
     * @param accessToken
     * @return
     */
    ImUser register(String accessToken);

}