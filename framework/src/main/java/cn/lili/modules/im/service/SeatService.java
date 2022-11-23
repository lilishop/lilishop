package cn.lili.modules.im.service;


import cn.lili.common.security.token.Token;
import cn.lili.modules.im.entity.dos.Seat;
import cn.lili.modules.im.entity.vo.SeatVO;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * 坐席业务
 *
 * @author pikachu
 * @since 2020-02-18 16:18:56
 */
public interface SeatService extends IService<Seat> {


    /**
     * 获取坐席列表
     *
     * @param storeId 店铺id
     * @return
     */
    List<SeatVO> seatVoList(String storeId);

    /**
     * 坐席登录
     *
     * @param username
     * @param password
     * @return
     */
    Token usernameLogin(String username, String password);

    /**
     * 快捷登录code 生成
     *
     * @param username 用户名
     * @return
     */
    String createQuickLoginCode(String username);

    /**
     * 快捷登录
     *
     * @param code
     * @return
     */
    Token quickLogin(String code);

    /**
     * 查询坐席
     *
     * @param username
     * @return
     */
    Seat findByUsername(String username);
}