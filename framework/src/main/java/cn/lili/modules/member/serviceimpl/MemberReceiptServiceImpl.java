package cn.lili.modules.member.serviceimpl;


import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.utils.BeanUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.entity.dos.MemberReceipt;
import cn.lili.modules.member.entity.vo.MemberReceiptAddVO;
import cn.lili.modules.member.entity.vo.MemberReceiptVO;
import cn.lili.modules.member.mapper.MemberReceiptMapper;
import cn.lili.modules.member.service.MemberReceiptService;
import cn.lili.modules.member.service.MemberService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;


/**
 * 会员发票业务层实现
 *
 * @author Chopper
 * @since 2021-03-29 14:10:16
 */
@Service
public class MemberReceiptServiceImpl extends ServiceImpl<MemberReceiptMapper, MemberReceipt> implements MemberReceiptService {
    @Autowired
    private MemberService memberService;

    @Override
    public IPage<MemberReceipt> getPage(MemberReceiptVO memberReceiptVO, PageVO pageVO) {
        return this.page(PageUtil.initPage(pageVO), memberReceiptVO.lambdaQueryWrapper());
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean addMemberReceipt(MemberReceiptAddVO memberReceiptAddVO, String memberId) {
        //校验发票抬头是否重复
        List<MemberReceipt> receipts = this.baseMapper.selectList(new QueryWrapper<MemberReceipt>()
                .eq("member_id", memberId)
                .eq("receipt_title", memberReceiptAddVO.getReceiptTitle())
        );
        if (!receipts.isEmpty()) {
            throw new ServiceException(ResultCode.USER_RECEIPT_REPEAT_ERROR);
        }
        //参数封装
        MemberReceipt memberReceipt = new MemberReceipt();
        BeanUtil.copyProperties(memberReceiptAddVO, memberReceipt);
        //根据会员信息查询会员
        Member member = memberService.getById(memberId);
        if (member != null) {
            memberReceipt.setMemberId(memberId);
            memberReceipt.setMemberName(member.getUsername());
            //设置发票默认
            List<MemberReceipt> list = this.baseMapper.selectList(new QueryWrapper<MemberReceipt>().eq("member_id", memberId));
            //如果当前会员只有一个发票则默认为默认发票，反之需要校验参数默认值，做一些处理
            if (list.isEmpty()) {
                memberReceipt.setIsDefault(1);
            } else {
                if (memberReceiptAddVO.getIsDefault().equals(1)) {
                    //如果参数传递新添加的发票信息为默认，则需要把其他发票置为非默认
                    this.update(new UpdateWrapper<MemberReceipt>().eq("member_id", memberId));
                    //设置当前发票信息为默认
                    memberReceipt.setIsDefault(memberReceiptAddVO.getIsDefault());
                } else {
                    memberReceiptAddVO.setIsDefault(0);
                }
            }
            return this.baseMapper.insert(memberReceipt) > 0 ? true : false;
        }
        throw new ServiceException(ResultCode.USER_RECEIPT_NOT_EXIST);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean editMemberReceipt(MemberReceiptAddVO memberReceiptAddVO, String memberId) {
        //根据会员id查询发票信息
        MemberReceipt memberReceiptDb = this.baseMapper.selectById(memberReceiptAddVO.getId());
        if (memberReceiptDb != null) {
            //检验是否有权限修改
            if (!memberReceiptDb.getMemberId().equals(memberId)) {
                throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
            }
            //校验发票抬头是否重复
            List<MemberReceipt> receipts = this.baseMapper.selectList(new QueryWrapper<MemberReceipt>()
                    .eq("member_id", memberId)
                    .eq("receipt_title", memberReceiptAddVO.getReceiptTitle())
                    .ne("id", memberReceiptAddVO.getId())
            );
            if (!receipts.isEmpty()) {
                throw new ServiceException(ResultCode.USER_RECEIPT_REPEAT_ERROR);
            }
            BeanUtil.copyProperties(memberReceiptAddVO, memberReceiptDb);
            //对发票默认进行处理  如果参数传递新添加的发票信息为默认，则需要把其他发票置为非默认
            if (memberReceiptAddVO.getIsDefault().equals(1)) {
                this.update(new UpdateWrapper<MemberReceipt>().eq("member_id", memberId));
            }
            return this.baseMapper.updateById(memberReceiptDb) > 0 ? true : false;
        }
        throw new ServiceException(ResultCode.USER_RECEIPT_NOT_EXIST);
    }

    @Override
    public Boolean deleteMemberReceipt(String id) {
        //根据会员id查询发票信息
        MemberReceipt memberReceiptDb = this.baseMapper.selectById(id);
        if (memberReceiptDb != null) {
            //如果会员发票信息不为空 则逻辑删除此发票信息
            memberReceiptDb.setDeleteFlag(false);
            this.baseMapper.updateById(memberReceiptDb);
        }
        return true;
    }
}