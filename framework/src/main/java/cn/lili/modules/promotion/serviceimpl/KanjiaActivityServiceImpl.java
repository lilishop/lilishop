package cn.lili.modules.promotion.serviceimpl;


import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.utils.CurrencyUtil;
import cn.lili.common.utils.PageUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.promotion.entity.dos.KanjiaActivity;
import cn.lili.modules.promotion.entity.dos.KanjiaActivityGoods;
import cn.lili.modules.promotion.entity.dos.KanjiaActivityLog;
import cn.lili.modules.promotion.entity.dto.KanjiaActivityDTO;
import cn.lili.modules.promotion.entity.dto.KanjiaActivityQuery;
import cn.lili.modules.promotion.entity.enums.KanJiaStatusEnum;
import cn.lili.modules.promotion.entity.enums.PromotionStatusEnum;
import cn.lili.modules.promotion.entity.vos.KanjiaActivitySearchParams;
import cn.lili.modules.promotion.mapper.KanJiaActivityMapper;
import cn.lili.modules.promotion.service.KanjiaActivityGoodsService;
import cn.lili.modules.promotion.service.KanjiaActivityLogService;
import cn.lili.modules.promotion.service.KanjiaActivityService;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;


/**
 * 砍价活动参与记录业务层实现
 *
 * @author qiuqiu
 * @date 2021/7/1
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class KanjiaActivityServiceImpl extends ServiceImpl<KanJiaActivityMapper, KanjiaActivity> implements KanjiaActivityService {

    @Autowired
    private KanjiaActivityGoodsService kanjiaActivityGoodsService;
    @Autowired
    private KanjiaActivityLogService kanjiaActivityLogService;
    @Autowired
    private MemberService memberService;
    @Autowired
    private GoodsSkuService goodsSkuService;

    @Override
    public KanjiaActivity getKanjiaActivity(KanjiaActivitySearchParams kanJiaActivitySearchParams) {
        return this.getOne(kanJiaActivitySearchParams.wrapper());
    }

    @Override
    public KanjiaActivityLog add(String id) {
        //根据skuId查询当前sku是否参与活动并且是在活动进行中
        KanjiaActivityGoods kanJiaActivityGoods = kanjiaActivityGoodsService.getById(id);
        //只有砍价商品存在且已经开始的活动才可以发起砍价
        if (kanJiaActivityGoods != null && kanJiaActivityGoods.getPromotionStatus().equals(PromotionStatusEnum.START.name())) {
            //获取会员信息
            Member member = memberService.getById(UserContext.getCurrentUser().getId());
            //校验此活动是否已经发起过
            QueryWrapper<KanjiaActivity> queryWrapper = new QueryWrapper<>();
            queryWrapper.eq("kan_jia_activity_goods_id", kanJiaActivityGoods.getId());
            queryWrapper.eq("member_id", member.getId());
            if (this.count(queryWrapper) > 0) {
                throw new ServiceException(ResultCode.KANJIA_ACTIVITY_MEMBER_ERROR);
            }
            KanjiaActivity kanJiaActivity = new KanjiaActivity();
            //获取商品信息
            GoodsSku goodsSku = goodsSkuService.getGoodsSkuByIdFromCache(kanJiaActivityGoods.getSkuId());
            if (goodsSku != null && member != null) {
                kanJiaActivity.setSkuId(kanJiaActivityGoods.getSkuId());
                kanJiaActivity.setGoodsName(goodsSku.getGoodsName());
                kanJiaActivity.setKanjiaActivityGoodsId(kanJiaActivityGoods.getId());
                kanJiaActivity.setThumbnail(goodsSku.getThumbnail());
                kanJiaActivity.setMemberId(UserContext.getCurrentUser().getId());
                kanJiaActivity.setMemberName(member.getUsername());
                kanJiaActivity.setStatus(KanJiaStatusEnum.START.name());
                //剩余砍价金额 开始 是商品金额-最低购买金额
                Double surplusPrice = CurrencyUtil.sub(goodsSku.getPrice(), kanJiaActivityGoods.getPurchasePrice());
                //获取砍价金额，因为自己要给自己砍一刀 所以需要在可砍金额范围内 砍一刀
                Double price = this.getKanjiaPrice(kanJiaActivityGoods, surplusPrice);
                //剩余可砍金额就是 砍一刀后的金额
                kanJiaActivity.setSurplusPrice(CurrencyUtil.sub(surplusPrice, price));
                //保存我的砍价活动
                boolean result = this.save(kanJiaActivity);
                //因为发起砍价就是自己给自己砍一刀，所以要添加砍价记录信息
                if (result) {
                    KanjiaActivityDTO kanjiaActivityDTO = new KanjiaActivityDTO();
                    kanjiaActivityDTO.setKanjiaActivityGoodsId(kanJiaActivityGoods.getId());
                    kanjiaActivityDTO.setKanjiaActivityId(kanJiaActivity.getId());
                    kanjiaActivityDTO.setKanjiaPrice(price);
                    kanjiaActivityDTO.setSurplusPrice(kanJiaActivity.getSurplusPrice());
                    kanjiaActivityDTO.setKanjiaMemberId(kanJiaActivity.getMemberId());
                    kanjiaActivityDTO.setKanjiaMemberName(kanJiaActivity.getMemberName());
                    kanjiaActivityDTO.setKanjiaMemberFace(member.getFace());
                    return kanjiaActivityLogService.addKanJiaActivityLog(kanjiaActivityDTO);
                }

            }
            throw new ServiceException(ResultCode.GOODS_NOT_EXIST);
        }
        throw new ServiceException(ResultCode.PROMOTION_STATUS_END);
    }


    @Override
    public KanjiaActivityLog helpKanJia(String kanjiaActivityId) {
        //获取会员信息
        Member member = memberService.getById(UserContext.getCurrentUser().getId());
        //根据砍价发起活动id查询砍价活动信息
        KanjiaActivity kanjiaActivity = this.getById(kanjiaActivityId);
        if (kanjiaActivity != null && kanjiaActivity.getStatus().equals(PromotionStatusEnum.START.name())) {
            if (member == null) {
                throw new ServiceException(ResultCode.USER_NOT_EXIST);
            }
            //根据skuId查询当前sku是否参与活动并且是在活动进行中
            KanjiaActivityGoods kanJiaActivityGoods = kanjiaActivityGoodsService.getById(kanjiaActivity.getKanjiaActivityGoodsId());
            if (kanJiaActivityGoods == null) {
                throw new ServiceException(ResultCode.PROMOTION_STATUS_END);
            }
            KanjiaActivityDTO kanjiaActivityDTO = new KanjiaActivityDTO();
            kanjiaActivityDTO.setKanjiaActivityGoodsId(kanjiaActivity.getKanjiaActivityGoodsId());
            kanjiaActivityDTO.setKanjiaActivityId(kanjiaActivityId);
            //获取砍价金额
            Double price = this.getKanjiaPrice(kanJiaActivityGoods, kanjiaActivity.getSurplusPrice());
            kanjiaActivityDTO.setKanjiaPrice(price);
            //获取可砍金额
            kanjiaActivityDTO.setSurplusPrice(CurrencyUtil.sub(kanjiaActivity.getSurplusPrice(), price));
            //如果可砍金额为0的话说明活动成功了
            if (Double.doubleToLongBits(kanjiaActivityDTO.getSurplusPrice()) == Double.doubleToLongBits(0D)) {
                kanjiaActivity.setStatus(KanJiaStatusEnum.SUCCESS.name());
                this.updateById(kanjiaActivity);
            }
            kanjiaActivityDTO.setKanjiaMemberId(member.getId());
            kanjiaActivityDTO.setKanjiaMemberName(member.getUsername());
            kanjiaActivityDTO.setKanjiaMemberFace(member.getFace());
            return kanjiaActivityLogService.addKanJiaActivityLog(kanjiaActivityDTO);
        }
        throw new ServiceException(ResultCode.PROMOTION_STATUS_END);
    }

    /**
     * 随机获取砍一刀价格
     *
     * @param kanjiaActivityGoods 砍价商品信息
     * @param surplusPrice        剩余可砍金额
     * @return
     */
    private Double getKanjiaPrice(KanjiaActivityGoods kanjiaActivityGoods, Double surplusPrice) {
        double result = kanjiaActivityGoods.getHighestPrice() - kanjiaActivityGoods.getLowestPrice();
        double num = (Math.random() * result);
        //如果剩余金额小于最低可砍价金额则直接返回最小金额 砍价成功
        if (kanjiaActivityGoods.getLowestPrice() > surplusPrice) {
            return surplusPrice;
        }
        Double price = 0D;
        while (true) {
            price = CurrencyUtil.round(num + kanjiaActivityGoods.getLowestPrice(), 2);
            //从最大金额和最小金额之间获取随机砍价金额
            if (price < surplusPrice) {
                break;
            } else {
                price = surplusPrice;
                break;
            }
        }
        return price;

    }


    @Override
    public IPage<KanjiaActivity> getForPage(KanjiaActivityQuery kanjiaActivityQuery, PageVO page) {
        QueryWrapper<KanjiaActivity> queryWrapper = kanjiaActivityQuery.wrapper();
        return this.page(PageUtil.initPage(page), queryWrapper);
    }

}