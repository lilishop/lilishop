package cn.lili.modules.member.serviceimpl;

import cn.lili.modules.member.entity.dos.MemberNoticeLog;
import cn.lili.modules.member.mapper.MemberNoticeLogMapper;
import cn.lili.modules.member.service.MemberNoticeLogService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

/**
 * 会员消息业务层实现
 *
 * @author Chopper
 * @since 2020/11/17 3:44 下午
 */
@Service
public class MemberNoticeLogServiceImpl extends ServiceImpl<MemberNoticeLogMapper, MemberNoticeLog> implements MemberNoticeLogService {
}