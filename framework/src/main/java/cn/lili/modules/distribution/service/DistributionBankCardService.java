package cn.lili.modules.distribution.service;

import cn.lili.modules.distribution.entity.dos.DistributionBankCard;
import cn.lili.modules.distribution.entity.dto.DistributionBankCardAddDTO;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * 分销员银行卡业务层
 *
 * @author Bulbasaur
 * @since 2026-08-06
 */
public interface DistributionBankCardService extends IService<DistributionBankCard> {

    /**
     * 查询当前分销员的银行卡列表。
     *
     * @return 银行卡列表
     */
    List<DistributionBankCard> listMyCards();

    /**
     * 添加银行卡
     *
     * @param dto 银行卡信息
     * @return 新增的银行卡
     */
    DistributionBankCard addCard(DistributionBankCardAddDTO dto);

    /**
     * 删除银行卡
     *
     * @param id 银行卡ID
     */
    void deleteCard(String id);

    /**
     * 设置默认银行卡
     *
     * @param id 银行卡ID
     */
    void setDefault(String id);
}
