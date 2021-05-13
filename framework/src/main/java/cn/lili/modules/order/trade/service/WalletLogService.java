package cn.lili.modules.order.trade.service;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.order.trade.entity.dos.WalletLog;
import cn.lili.modules.order.trade.entity.vo.DepositQueryVO;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

/**
 * 预存款日志业务层
 *
 * @author pikachu
 * @date 2020-02-25 14:10:16
 */
public interface WalletLogService extends IService<WalletLog> {


    /**
     * 预存款充值日志记录
     *
     * @param page           分页数据
     * @param depositQueryVO 查询条件
     * @return 日志记录分页列表
     */
    IPage<WalletLog> depositLogPage(PageVO page, DepositQueryVO depositQueryVO);

}