package cn.lili.modules.member.serviceimpl;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.rocketmq.RocketmqSendCallbackBuilder;
import cn.lili.common.rocketmq.tags.MemberTagsEnum;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.utils.CurrencyUtil;
import cn.lili.common.utils.DateUtil;
import cn.lili.common.utils.StringUtils;
import cn.lili.config.rocketmq.RocketmqCustomProperties;
import cn.lili.modules.member.entity.dos.MemberSign;
import cn.lili.modules.member.mapper.MemberSignMapper;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.member.service.MemberSignService;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.PointSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.google.gson.Gson;
import org.apache.rocketmq.spring.core.RocketMQTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Date;
import java.util.List;
import java.util.Map;

/**
 * 会员签到业务层实现
 *
 * @author pikachu
 * @date 2020-02-25 14:10:16
 */
@Service
public class MemberSignServiceImpl extends ServiceImpl<MemberSignMapper, MemberSign> implements MemberSignService {

    /**
     * RocketMQ
     */
    @Autowired
    private RocketMQTemplate rocketMQTemplate;
    /**
     * RocketMQ 配置
     */
    @Autowired
    private RocketmqCustomProperties rocketmqCustomProperties;
    /**
     * 配置
     */
    @Autowired
    private SettingService settingService;
    /**
     * 会员
     */
    @Autowired
    private MemberService memberService;


    @Override
    public Boolean memberSign() {
        //获取当前会员信息
        AuthUser authUser = UserContext.getCurrentUser();
        if (authUser != null) {
            QueryWrapper<MemberSign> queryWrapper = new QueryWrapper<>();
            queryWrapper.eq("member_id",authUser.getId());
            queryWrapper.between("create_time",new Date(DateUtil.startOfTodDay()*1000),DateUtil.getCurrentDayEndTime());
            //校验今天是否已经签到
            List<MemberSign> todaySigns = this.baseMapper.getTodayMemberSign(queryWrapper);
            if (todaySigns.size() > 0) {
                throw new ServiceException(ResultCode.MEMBER_SIGN_REPEAT);
            }
            //当前签到天数的前一天日期
            List<MemberSign> signs = this.baseMapper.getBeforeMemberSign(authUser.getId());
            //构建参数
            MemberSign memberSign = new MemberSign();
            memberSign.setMemberId(authUser.getId());
            memberSign.setMemberName(authUser.getUsername());
            //如果size大于0 说明昨天已经签到过，获取昨天的签到数，反之新签到
            if (signs.size() > 0) {
                //截止目前为止 签到总天数 不带今天
                Integer signDay = signs.get(0).getSignDay();
                memberSign.setSignDay(CurrencyUtil.add(signDay, 1).intValue());
            } else {
                memberSign.setSignDay(1);
            }
            Integer result = this.baseMapper.insert(memberSign);
            //签到成功后发送消息赠送积分
            if (result > 0) {
                String destination = rocketmqCustomProperties.getMemberTopic() + ":" + MemberTagsEnum.MEMBER_SING.name();
                rocketMQTemplate.asyncSend(destination, memberSign, RocketmqSendCallbackBuilder.commonCallback());
                return true;
            }
            return false;
        }
        throw new ServiceException(ResultCode.USER_NOT_LOGIN);
    }

    @Override
    public List<MemberSign> getMonthSignDay(String time) {
        //获取当前会员
        AuthUser authUser = UserContext.getCurrentUser();
        if (authUser != null) {
            return this.baseMapper.getMonthMemberSign(authUser.getId(), time);
        }
        throw new ServiceException(ResultCode.USER_NOT_LOGIN);
    }

    @Override
    public void memberSignSendPoint(String memberId, Integer day) {
        try {
            //获取签到积分赠送设置
            Setting setting = settingService.get(SettingEnum.POINT_SETTING.name());
            if (setting != null) {
                PointSetting pointSetting = new Gson().fromJson(setting.getSettingValue(), PointSetting.class);
                String content = "";
                Long point = -1L;
                //将对象转换成map 方便签到天数和签到积分数获取，方便扩展
                Map map = StringUtils.objectToMap(pointSetting);
                for (int i = 1; i <= 4; i++) {
                    //连续签到数
                    Object dayObj = map.get("signIn" + i);
                    //连续签到赠送积分
                    Object pointObj = map.get("signIn" + i + "Point");
                    if (dayObj != null && pointObj != null) {
                        //如果当前连续赠送天数等于设置连续赠送天数 则记录赠送积分数
                        if (dayObj == day) {
                            point = Long.valueOf(map.get("signIn" + i + "Point").toString());
                            content = "会员连续签到" + day + "天，赠送积分" + point + "分";
                        }
                    }
                }
                //如果他不处于连续赠送阶段，则只赠送签到积分数
                if (point == -1 && pointSetting.getSignIn() != null) {
                    point = Long.valueOf(pointSetting.getSignIn().toString());
                    content = "会员签到第" + day + "天，赠送积分" + point + "分";
                }
                //赠送会员积分
                memberService.updateMemberPoint(point, true, memberId, content);
            }
        } catch (Exception e) {
            log.error("会员签到错误",e);
        }
    }

}