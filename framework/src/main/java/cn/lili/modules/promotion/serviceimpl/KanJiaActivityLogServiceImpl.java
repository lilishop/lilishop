package cn.lili.modules.promotion.serviceimpl;


import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.utils.BeanUtil;
import cn.lili.common.utils.PageUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.promotion.entity.dos.KanJiaActivity;
import cn.lili.modules.promotion.entity.dos.KanJiaActivityGoods;
import cn.lili.modules.promotion.entity.dos.KanJiaActivityLog;
import cn.lili.modules.promotion.entity.dto.KanJiaActivityDTO;
import cn.lili.modules.promotion.entity.dto.KanJiaActivityLogQuery;
import cn.lili.modules.promotion.entity.enums.PromotionStatusEnum;
import cn.lili.modules.promotion.mapper.KanJiaActivityLogMapper;
import cn.lili.modules.promotion.service.KanJiaActivityGoodsService;
import cn.lili.modules.promotion.service.KanJiaActivityLogService;
import cn.lili.modules.promotion.service.KanJiaActivityService;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;


/**
 * 砍价活动日志业务层实现
 *
 * @author qiuqiu
 * @date 2021/7/1
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class KanJiaActivityLogServiceImpl extends ServiceImpl<KanJiaActivityLogMapper, KanJiaActivityLog> implements KanJiaActivityLogService {

    @Autowired
    private KanJiaActivityGoodsService kanJiaActivityGoodsService;

    @Autowired
    private KanJiaActivityService kanJiaActivityService;

    @Override
    public IPage<KanJiaActivityLog> getForPage(KanJiaActivityLogQuery kanJiaActivityLogQuery, PageVO pageVO) {
        QueryWrapper<KanJiaActivityLog> queryWrapper = kanJiaActivityLogQuery.wrapper();
        return this.page(PageUtil.initPage(pageVO), queryWrapper);
    }


    @Override
    public KanJiaActivityLog addKanJiaActivityLog(KanJiaActivityDTO kanJiaActivityDTO) {
        //校验当前会员是否已经参与过此次砍价
        QueryWrapper<KanJiaActivityLog> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq(kanJiaActivityDTO.getKanJiaMemberId() != null, "kan_jia_member_id", kanJiaActivityDTO.getKanJiaMemberId());
        queryWrapper.eq(kanJiaActivityDTO.getKanJiaActivityId() != null, "kan_jia_activity_id", kanJiaActivityDTO.getKanJiaActivityId());
        Integer count = this.baseMapper.selectCount(queryWrapper);
        if (count > 0) {
            throw new ServiceException(ResultCode.KANJIA_ACTIVITY_LOG_MEMBER_ERROR);
        }
        //校验当前砍价商品是否有效
        KanJiaActivityGoods kanJiaActivityGoods = kanJiaActivityGoodsService.getById(kanJiaActivityDTO.getKanJiaActivityGoodsId());
        //如果当前活动不为空且还在活动时间内 才可以参与砍价活动
        if (kanJiaActivityGoods != null && kanJiaActivityGoods.getPromotionStatus().equals(PromotionStatusEnum.START.name())) {
            //获取砍价参与者记录
            KanJiaActivity kanJiaActivity = kanJiaActivityService.getById(kanJiaActivityDTO.getKanJiaActivityId());
            if (kanJiaActivity != null) {
                KanJiaActivityLog kanJiaActivityLog = new KanJiaActivityLog();
                kanJiaActivityLog.setKanJiaActivityId(kanJiaActivity.getId());
                BeanUtil.copyProperties(kanJiaActivityDTO, kanJiaActivityLog);
                boolean result = this.save(kanJiaActivityLog);
                if (result) {
                    return kanJiaActivityLog;
                }
            }
            throw new ServiceException(ResultCode.KANJIA_ACTIVITY_NOT_FOUND_ERROR);
        }
        throw new ServiceException(ResultCode.PROMOTION_STATUS_END);

    }
}