package cn.lili.im.serviceimpl;

import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.im.entity.ImUser;
import cn.lili.im.mapper.ImUserMapper;
import cn.lili.im.service.ImUserService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * Im消息 业务实现
 *
 * @author Chopper
 */
@Service
@Transactional(rollbackFor = Exception.class)
@RequiredArgsConstructor(onConstructor = @__(@Autowired))
public class ImUserServiceImpl extends ServiceImpl<ImUserMapper, ImUser> implements ImUserService {

    @Override
    public ImUser register(String accessToken) {
        AuthUser authUser = UserContext.getAuthUser(accessToken);
        ImUser imUser;
        //如果用户存在
        imUser = this.getById(authUser.getId());
        if (imUser == null) {
            imUser = new ImUser();
            imUser.setId(authUser.getId());
            imUser.setName(authUser.getNickName());
            this.save(imUser);
        }
        return imUser;
    }

}