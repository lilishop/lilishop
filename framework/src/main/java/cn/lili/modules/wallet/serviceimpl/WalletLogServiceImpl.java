package cn.lili.modules.wallet.serviceimpl;

import cn.lili.common.utils.StringUtils;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.order.trade.entity.vo.DepositQueryVO;
import cn.lili.modules.wallet.entity.dos.WalletLog;
import cn.lili.modules.wallet.mapper.WalletLogMapper;
import cn.lili.modules.wallet.service.WalletLogService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Date;

/**
 * 预存款日志业务层实现
 *
 * @author pikachu
 * @since 2020-02-25 14:10:16
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class WalletLogServiceImpl extends ServiceImpl<WalletLogMapper, WalletLog> implements WalletLogService {

    @Override
    public IPage<WalletLog> depositLogPage(PageVO page, DepositQueryVO depositQueryVO) {
        //构建查询条件
        QueryWrapper<WalletLog> depositLogQueryWrapper = new QueryWrapper<>();
        //会员名称
        depositLogQueryWrapper.like(!StringUtils.isEmpty(depositQueryVO.getMemberName()), "member_name", depositQueryVO.getMemberName());
        //会员id
        depositLogQueryWrapper.eq(!StringUtils.isEmpty(depositQueryVO.getMemberId()), "member_id", depositQueryVO.getMemberId());
        //开始时间和技术时间
        if (!StringUtils.isEmpty(depositQueryVO.getStartDate()) && !StringUtils.isEmpty(depositQueryVO.getEndDate())) {
            Date start = cn.hutool.core.date.DateUtil.parse(depositQueryVO.getStartDate());
            Date end = cn.hutool.core.date.DateUtil.parse(depositQueryVO.getEndDate());
            depositLogQueryWrapper.between("create_time", start, end);
        }
        //查询返回数据
        return this.page(PageUtil.initPage(page), depositLogQueryWrapper);
    }
}