package cn.lili.modules.im.serviceimpl;

import cn.lili.cache.Cache;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.token.Token;
import cn.lili.common.utils.StringUtils;
import cn.lili.modules.im.entity.dos.Seat;
import cn.lili.modules.im.entity.enums.OnlineStatusEnum;
import cn.lili.modules.im.entity.vo.SeatVO;
import cn.lili.modules.im.mapper.SeatMapper;
import cn.lili.modules.im.service.SeatService;
import cn.lili.modules.im.token.SeatTokenGenerate;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

/**
 * 坐席业务层实现
 *
 * @author pikachu
 * @since 2020-02-18 16:18:56
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class SeatServiceImpl extends ServiceImpl<SeatMapper, Seat> implements SeatService {


    @Autowired
    private SeatTokenGenerate seatTokenGenerate;

    @Autowired
    private Cache<String> cache;

    /**
     * 快捷登录缓存前缀
     */
    private static String prefix = "{quick_login}_";


    @Override
    public List<SeatVO> seatVoList(String storeId) {

        LambdaQueryWrapper<Seat> seatLambdaQueryWrapper = new LambdaQueryWrapper<>();
        seatLambdaQueryWrapper.eq(Seat::getTenantId, storeId);
        List<Seat> list = this.list(seatLambdaQueryWrapper);

        //转换模型为VO
        List<SeatVO> results = list.stream().map(item -> (SeatVO) item).collect(Collectors.toList());
        //填充坐席当前状态
        //todo
        results.forEach(item -> {
            item.setOnlineStatus(OnlineStatusEnum.ONLINE.name());
        });
        return results;
    }

    @Override
    public Token usernameLogin(String username, String password) {

        Seat seat = this.findByUsername(username);
        //判断用户是否存在
        if (seat == null || !seat.getDisabled()) {
            throw new ServiceException(ResultCode.ERROR);
        }
        //判断密码是否输入正确
        if (!new BCryptPasswordEncoder().matches(password, seat.getPassword())) {
            throw new ServiceException(ResultCode.ERROR);
        }
        return seatTokenGenerate.createToken(seat, true);
    }

    @Override
    public String createQuickLoginCode(String username) {
        String code = UUID.randomUUID().toString();
        cache.put(prefix + code, username, 20L);
        return code;
    }

    @Override
    public Token quickLogin(String code) {
        String username = cache.get(prefix + code);
        cache.remove(prefix + code);
        if (StringUtils.isEmpty(username)) {
            throw new ServiceException(ResultCode.ERROR);
        }
        return seatTokenGenerate.createToken(findByUsername(username), true);
    }

    /**
     * 查询坐席
     *
     * @param username
     * @return
     */
    @Override
    public Seat findByUsername(String username) {
        LambdaQueryWrapper<Seat> seatLambdaQueryWrapper = new LambdaQueryWrapper<>();
        seatLambdaQueryWrapper.eq(Seat::getUsername, username);
        return this.getOne(seatLambdaQueryWrapper);
    }


}