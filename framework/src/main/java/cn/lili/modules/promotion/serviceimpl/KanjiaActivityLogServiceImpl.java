package cn.lili.modules.promotion.serviceimpl;


import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.utils.BeanUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.promotion.entity.dos.KanjiaActivity;
import cn.lili.modules.promotion.entity.dos.KanjiaActivityGoods;
import cn.lili.modules.promotion.entity.dos.KanjiaActivityLog;
import cn.lili.modules.promotion.entity.dto.KanjiaActivityDTO;
import cn.lili.modules.promotion.entity.dto.search.KanJiaActivityLogQuery;
import cn.lili.modules.promotion.entity.enums.PromotionsStatusEnum;
import cn.lili.modules.promotion.mapper.KanJiaActivityLogMapper;
import cn.lili.modules.promotion.mapper.KanJiaActivityMapper;
import cn.lili.modules.promotion.service.KanjiaActivityGoodsService;
import cn.lili.modules.promotion.service.KanjiaActivityLogService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.stereotype.Service;


/**
 * 砍价活动日志业务层实现
 * 参与记录仅通过存储层读取，避免日志服务反向依赖调用它的砍价参与服务。
 *
 * @author qiuqiu
 * @date 2021/7/1
 */
@Service
public class KanjiaActivityLogServiceImpl extends ServiceImpl<KanJiaActivityLogMapper, KanjiaActivityLog> implements KanjiaActivityLogService {

    @Autowired
    private KanjiaActivityGoodsService kanJiaActivityGoodsService;
    @Autowired
    private KanJiaActivityMapper kanjiaActivityMapper;

    @Override
    public IPage<KanjiaActivityLog> getForPage(KanJiaActivityLogQuery kanJiaActivityLogQuery, PageVO pageVO) {
        QueryWrapper<KanjiaActivityLog> queryWrapper = kanJiaActivityLogQuery.wrapper();
        return this.page(PageUtil.initPage(pageVO), queryWrapper);
    }


    /**
     * 校验当前用户未重复参与、商品活动已开始且参与记录存在后保存砍价日志。
     * 参与记录查询沿用逻辑删除规则；唯一键冲突转换为重复参与提示，事务沿用调用方。
     *
     * @param kanjiaActivityDTO 本次砍价金额、剩余金额和参与者信息
     * @return 保存成功的砍价日志
     * @throws ServiceException 重复参与、活动未开始或已结束、参与记录缺失或保存失败时抛出
     */
    @Override
    public KanjiaActivityLog addKanJiaActivityLog(KanjiaActivityDTO kanjiaActivityDTO) {
        //校验当前客户是否已经参与过此次砍价
        LambdaQueryWrapper<KanjiaActivityLog> queryWrapper = new LambdaQueryWrapper<KanjiaActivityLog>();
        queryWrapper.eq(kanjiaActivityDTO.getKanjiaActivityId() != null, KanjiaActivityLog::getKanjiaActivityId, kanjiaActivityDTO.getKanjiaActivityId());
        queryWrapper.eq(KanjiaActivityLog::getKanjiaMemberId, UserContext.getCurrentUser().getId());
        long count = this.baseMapper.selectCount(queryWrapper);
        if (count > 0) {
            throw new ServiceException(ResultCode.KANJIA_ACTIVITY_LOG_MEMBER_ERROR);
        }
        //校验当前砍价商品是否有效
        KanjiaActivityGoods kanjiaActivityGoods = kanJiaActivityGoodsService.getById(kanjiaActivityDTO.getKanjiaActivityGoodsId());
        //如果当前活动不为空且还在活动时间内 才可以参与砍价活动
        if (kanjiaActivityGoods != null && kanjiaActivityGoods.getPromotionStatus().equals(PromotionsStatusEnum.START.name())) {
            //获取砍价参与者记录
            KanjiaActivity kanjiaActivity = kanjiaActivityMapper.selectById(kanjiaActivityDTO.getKanjiaActivityId());
            if (kanjiaActivity != null) {
                KanjiaActivityLog kanJiaActivityLog = new KanjiaActivityLog();
                kanJiaActivityLog.setKanjiaActivityId(kanjiaActivity.getId());
                BeanUtil.copyProperties(kanjiaActivityDTO, kanJiaActivityLog);
                boolean result;
                try {
                    result = this.save(kanJiaActivityLog);
                } catch (DuplicateKeyException e) {
                    throw new ServiceException(ResultCode.KANJIA_ACTIVITY_LOG_MEMBER_ERROR);
                }
                if (result) {
                    return kanJiaActivityLog;
                }
            }
            throw new ServiceException(ResultCode.KANJIA_ACTIVITY_NOT_FOUND_ERROR);
        }
        throw new ServiceException(ResultCode.PROMOTION_STATUS_END);

    }
}
