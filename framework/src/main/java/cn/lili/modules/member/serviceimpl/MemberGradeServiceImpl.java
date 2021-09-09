package cn.lili.modules.member.serviceimpl;

import cn.lili.modules.member.entity.dos.MemberGrade;
import cn.lili.modules.member.mapper.MemberGradeMapper;
import cn.lili.modules.member.service.MemberGradeService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

/**
 * 会员等级业务层实现
 *
 * @author Bulbasaur
 * @since 2021/5/14 5:58 下午
 */
@Service
public class MemberGradeServiceImpl extends ServiceImpl<MemberGradeMapper, MemberGrade> implements MemberGradeService {

}