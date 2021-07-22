package cn.lili.modules.member.serviceimpl;

import cn.lili.modules.member.entity.dos.MemberNotice;
import cn.lili.modules.member.mapper.MemberNoticeMapper;
import cn.lili.modules.member.service.MemberNoticeService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

/**
 * 会员站内信业务层实现
 *
 * @author Chopper
 * @since 2020/11/17 3:44 下午
 */
@Service
public class MemberNoticeServiceImpl extends ServiceImpl<MemberNoticeMapper, MemberNotice> implements MemberNoticeService {

}