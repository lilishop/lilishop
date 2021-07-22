package cn.lili.modules.member.service;

import cn.lili.modules.member.entity.dos.MemberSign;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * 会员签到业务层
 *
 * @author pikachu
 * @since 2020-02-25 14:10:16
 */
public interface MemberSignService extends IService<MemberSign> {

    /**
     * 会员签到
     *
     * @return 是否签到成功
     */
    Boolean memberSign();

    /**
     * 根据时间查询签到数据
     * 主要是根据年月查询，给会员端日历展示数据
     *
     * @param time 时间 格式 YYYYmm
     * @return 会员签到列表
     */
    List<MemberSign> getMonthSignDay(String time);

    /**
     * 会员签到赠送积分
     *
     * @param memberId 会员id
     * @param day      签到天数
     */
    void memberSignSendPoint(String memberId, Integer day);


}