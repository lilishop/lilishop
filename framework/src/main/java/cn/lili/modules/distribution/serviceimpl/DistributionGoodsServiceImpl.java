package cn.lili.modules.distribution.serviceimpl;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.json.JSONUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.modules.distribution.entity.dos.Distribution;
import cn.lili.modules.distribution.entity.dos.DistributionGoods;
import cn.lili.modules.distribution.entity.dto.DistributionGoodsAuditDTO;
import cn.lili.modules.distribution.entity.dto.DistributionGoodsBatchCheckDTO;
import cn.lili.modules.distribution.entity.dto.DistributionGoodsCheckItemDTO;
import cn.lili.modules.distribution.entity.dto.DistributionGoodsSearchParams;
import cn.lili.modules.distribution.entity.enums.DistributionGoodsAuthEnum;
import cn.lili.modules.distribution.entity.enums.DistributionStatusEnum;
import cn.lili.modules.distribution.entity.vos.DistributionGoodsPosterVO;
import cn.lili.modules.distribution.entity.vos.DistributionGoodsShareVO;
import cn.lili.modules.distribution.entity.vos.DistributionGoodsVO;
import cn.lili.modules.distribution.mapper.DistributionGoodsMapper;
import cn.lili.modules.distribution.service.DistributionGoodsService;
import cn.lili.modules.distribution.service.DistributionService;
import cn.lili.modules.distribution.service.DistributionShareEventService;
import cn.lili.modules.distribution.support.DistributionBuyerCommissionSupport;
import cn.lili.modules.distribution.support.DistributionGoodsSettingSupport;
import cn.lili.modules.goods.entity.dos.Category;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.vos.GoodsSkuVO;
import cn.lili.modules.goods.service.CategoryService;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.BaseSetting;
import cn.lili.modules.system.entity.dto.DistributionFeaturedGoodsSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import cn.lili.modules.wechat.util.WechatMpCodeUtil;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;


/**
 * 分销商品接口实现
 *
 * @author pikachu
 * @since 2020-03-24 23:04:56
 */
@Service
@Slf4j
public class DistributionGoodsServiceImpl extends ServiceImpl<DistributionGoodsMapper, DistributionGoods> implements DistributionGoodsService {

    /**
     * 分销员
     */
    @Autowired
    private DistributionService distributionService;
    /**
     * 规格商品
     */
    @Autowired
    private GoodsSkuService goodsSkuService;

    @Autowired
    private CategoryService categoryService;

    @Autowired
    private DistributionGoodsSettingSupport distributionGoodsSettingSupport;

    @Autowired
    private SettingService settingService;

    @Autowired
    private WechatMpCodeUtil wechatMpCodeUtil;

    @Autowired
    private DistributionBuyerCommissionSupport buyerCommissionSupport;

    @Autowired
    private DistributionShareEventService shareEventService;

    private static final String SHARE_TRIGGER_PAGE = "distribution_share";
    private static final String SHARE_BUTTON_GENERATE_POSTER = "GENERATE_POSTER";

    @Override
    public IPage<DistributionGoodsVO> goodsPage(DistributionGoodsSearchParams searchParams) {
        IPage<DistributionGoodsVO> page;
        //获取商家的分销商品列表
        if (Objects.requireNonNull(UserContext.getCurrentUser()).getRole().equals(UserEnums.STORE)) {
            page = this.baseMapper.getDistributionGoodsVO(PageUtil.initPage(searchParams), searchParams.storeQueryWrapper());
        } else if (UserContext.getCurrentUser().getRole().equals(UserEnums.MEMBER)) {
            // 买家端：仅展示已审核商品，按配置排序并叠加等级佣金
            Distribution distribution = distributionService.getDistribution();
            if (distribution != null) {
                QueryWrapper<DistributionGoodsVO> memberWrapper = searchParams.distributionQueryWrapper();
                memberWrapper.eq("dg.auth_status", DistributionGoodsAuthEnum.PASS.name());
                distributionGoodsSettingSupport.applyMemberSort(
                        memberWrapper, searchParams.getSortField(), searchParams.getSortOrder());
                page = this.baseMapper.getDistributionGoodsVO(PageUtil.initPage(searchParams), memberWrapper);
                buyerCommissionSupport.applyToGoods(page.getRecords(), distribution);
            } else {
                throw new ServiceException(ResultCode.DISTRIBUTION_NOT_EXIST);
            }
        } else {
            //如果是平台则直接进行查询
            page = this.baseMapper.getDistributionGoodsVO(PageUtil.initPage(searchParams), searchParams.distributionQueryWrapper());
        }
        enrichPlatformCategoryName(page.getRecords());
        return page;
    }

    /**
     * 根据条件查询分销商品信息列表
     *
     * @param distributionGoodsSearchParams 商品条件
     * @return 分销商品信息列表
     */
    @Override
    public List<DistributionGoods> getDistributionGoodsList(DistributionGoodsSearchParams distributionGoodsSearchParams) {
        return this.list(distributionGoodsSearchParams.queryWrapper());
    }

    /**
     * 根据条件查询分销商品信息
     *
     * @param distributionGoodsSearchParams 条件
     * @return 分销商品信息
     */
    @Override
    public DistributionGoods getDistributionGoods(DistributionGoodsSearchParams distributionGoodsSearchParams) {
        return this.getOne(distributionGoodsSearchParams.queryWrapper(), false);
    }

    /**
     * 根据条件删除分销商品
     *
     * @param distributionGoodsSearchParams 条件
     */
    @Override
    public boolean deleteDistributionGoods(DistributionGoodsSearchParams distributionGoodsSearchParams) {
        return this.remove(distributionGoodsSearchParams.queryWrapper());
    }

    @Override
    public DistributionGoods distributionGoodsVO(String id) {

        return this.getById(id);
    }

    @Override
    public DistributionGoods distributionGoodsVOBySkuId(String skuId) {
        return this.getOne(new LambdaUpdateWrapper<DistributionGoods>().eq(DistributionGoods::getSkuId, skuId));
    }

    @Override
    public List<DistributionGoods> distributionGoods(List<String> skuIds) {
        return this.list(new LambdaUpdateWrapper<DistributionGoods>().in(DistributionGoods::getSkuId, skuIds));
    }

    @Override
    public DistributionGoods checked(String skuId, Double commission, String storeId) {

        //检查分销功能开关
        distributionService.checkDistributionSetting();

        //判断是否存在分销商品，已存在则直接跳过
        QueryWrapper queryWrapper = Wrappers.query().eq("sku_id", skuId);
        DistributionGoods existing = this.getOne(queryWrapper);
        if (existing != null) {
            return existing;
        }
        GoodsSku goodsSku = goodsSkuService.getGoodsSkuByIdFromCache(skuId);
        if (!goodsSku.getStoreId().equals(storeId)) {
            throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
        }
        DistributionGoods distributionGoods = new DistributionGoods(goodsSku, commission);
        this.save(distributionGoods);
        return distributionGoods;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public List<DistributionGoods> batchChecked(DistributionGoodsBatchCheckDTO batchDTO, String storeId) {
        distributionService.checkDistributionSetting();

        List<DistributionGoodsCheckItemDTO> items = batchDTO.getItems();
        // 按 SKU 去重，保留最后一次提交的佣金配置
        Map<String, DistributionGoodsCheckItemDTO> itemMap = new LinkedHashMap<>();
        for (DistributionGoodsCheckItemDTO item : items) {
            itemMap.put(item.getSkuId(), item);
        }
        List<String> skuIds = new ArrayList<>(itemMap.keySet());
        if (skuIds.isEmpty()) {
            return new ArrayList<>();
        }

        // 过滤已存在的分销商品，仅新增未入库的 SKU
        Set<String> existingSkuIds = this.list(Wrappers.<DistributionGoods>lambdaQuery()
                        .in(DistributionGoods::getSkuId, skuIds))
                .stream()
                .map(DistributionGoods::getSkuId)
                .collect(Collectors.toSet());

        List<String> toAddSkuIds = skuIds.stream()
                .filter(skuId -> !existingSkuIds.contains(skuId))
                .collect(Collectors.toList());
        if (toAddSkuIds.isEmpty()) {
            return new ArrayList<>();
        }

        List<GoodsSku> goodsSkus = goodsSkuService.listByIdsAndStoreId(toAddSkuIds, storeId);
        // 返回数量不一致说明存在非本店 SKU
        if (goodsSkus.size() != toAddSkuIds.size()) {
            throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
        }

        Map<String, GoodsSku> skuMap = goodsSkus.stream()
                .collect(Collectors.toMap(GoodsSku::getId, Function.identity(), (a, b) -> a));

        List<DistributionGoods> distributionGoodsList = new ArrayList<>();
        for (String skuId : toAddSkuIds) {
            GoodsSku goodsSku = skuMap.get(skuId);
            DistributionGoodsCheckItemDTO item = itemMap.get(skuId);
            if (goodsSku == null || item == null) {
                throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
            }
            distributionGoodsList.add(new DistributionGoods(goodsSku, item.getCommission()));
        }
        if (!distributionGoodsList.isEmpty()) {
            this.saveBatch(distributionGoodsList);
        }
        return distributionGoodsList;
    }

    @Override
    public DistributionGoods updateCommission(DistributionGoods distributionGoods, Double commission) {
        distributionGoods.setCommission(commission);
        this.updateById(distributionGoods);
        return distributionGoods;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void auditDistributionGoods(DistributionGoodsAuditDTO auditDTO) {
        if (!DistributionGoodsAuthEnum.PASS.name().equals(auditDTO.getAuthStatus())
                && !DistributionGoodsAuthEnum.REFUSE.name().equals(auditDTO.getAuthStatus())) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "审核状态不正确");
        }
        if (DistributionGoodsAuthEnum.REFUSE.name().equals(auditDTO.getAuthStatus())
                && CharSequenceUtil.isBlank(auditDTO.getAuthRemark())) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "请填写审核备注");
        }
        List<DistributionGoods> goodsList = listByIds(auditDTO.getIds());
        if (goodsList == null || goodsList.isEmpty()) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "分销商品不存在");
        }
        for (DistributionGoods goods : goodsList) {
            if (!isApprovedStatusPending(goods.getAuthStatus())) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "仅待审核的分销商品可审核");
            }
            goods.setAuthStatus(auditDTO.getAuthStatus());
            // 拒绝时保留备注，通过时清空备注
            goods.setAuthRemark(DistributionGoodsAuthEnum.REFUSE.name().equals(auditDTO.getAuthStatus())
                    ? auditDTO.getAuthRemark() : null);
        }
        updateBatchById(goodsList);
    }

    public static boolean isApproved(DistributionGoods goods) {
        if (goods == null) {
            return false;
        }
        return goods.getAuthStatus() == null
                || DistributionGoodsAuthEnum.PASS.name().equals(goods.getAuthStatus());
    }

    @Override
    public DistributionGoodsShareVO getShareContext(String skuId, String goodsId, String distributionId) {
        ShareAccessContext accessContext = resolveShareAccess(skuId, goodsId, distributionId);
        DistributionGoods distributionGoods = accessContext.distributionGoods();

        DistributionFeaturedGoodsSetting setting = distributionGoodsSettingSupport.readSetting();
        String defaultShareText = setting.getDefaultShareText();
        if (CharSequenceUtil.isBlank(defaultShareText)) {
            defaultShareText = "分享这个商品给你，快来一起购买吧～";
        }

        GoodsSkuVO goodsSkuVO = loadGoodsSkuVO(goodsId, skuId);
        String goodsName = CharSequenceUtil.blankToDefault(distributionGoods.getGoodsName(),
                goodsSkuVO == null ? "" : goodsSkuVO.getGoodsName());
        // 分销商品未设价时，优先取促销价，否则取原价
        Double price = distributionGoods.getPrice();
        if (price == null && goodsSkuVO != null) {
            price = Boolean.TRUE.equals(goodsSkuVO.getPromotionFlag()) && goodsSkuVO.getPromotionPrice() != null
                    ? goodsSkuVO.getPromotionPrice()
                    : goodsSkuVO.getPrice();
        }
        String thumbnail = CharSequenceUtil.blankToDefault(distributionGoods.getThumbnail(),
                goodsSkuVO == null ? null : goodsSkuVO.getThumbnail());
        List<String> images = resolveShareImages(distributionGoods, goodsSkuVO);
        String sharePath = buildSharePath(skuId, goodsId, distributionId);

        DistributionGoodsShareVO shareVO = new DistributionGoodsShareVO();
        shareVO.setDefaultShareText(defaultShareText);
        shareVO.setGoodsName(goodsName);
        shareVO.setGoodsSellingPoint(goodsSkuVO == null ? null : goodsSkuVO.getSellingPoint());
        shareVO.setPrice(price);
        shareVO.setThumbnail(thumbnail);
        shareVO.setImages(images);
        shareVO.setSharePath(sharePath);
        String miniProgramLink = buildShareLink(skuId, goodsId, distributionId, sharePath);
        shareVO.setMiniProgramLink(miniProgramLink);
        // 根据链接域名区分小程序 URL Link 与 H5 回退链接
        shareVO.setLinkSource(miniProgramLink != null && miniProgramLink.contains("wxmpurl.cn") ? "WXMP_URL" : "H5");
        return shareVO;
    }

    @Override
    public DistributionGoodsPosterVO getPosterContext(String skuId, String goodsId, String distributionId) {
        resolveShareAccess(skuId, goodsId, distributionId);
        String shareId = shareEventService.recordGoodsShare(
                distributionId,
                goodsId,
                skuId,
                "WECHAT_MINI_PROGRAM",
                SHARE_TRIGGER_PAGE,
                SHARE_BUTTON_GENERATE_POSTER
        );
        // scene 携带 sku、商品、分销员及分享事件 ID，供扫码落地页解析
        String scene = skuId + "," + goodsId + "," + distributionId + "," + shareId;
        String qrCode = wechatMpCodeUtil.createCode("pages/product/goods", scene);
        if (CharSequenceUtil.isBlank(qrCode)) {
            throw new ServiceException(ResultCode.ERROR);
        }
        DistributionGoodsPosterVO posterVO = new DistributionGoodsPosterVO();
        posterVO.setQrCode(qrCode);
        posterVO.setShareId(shareId);
        return posterVO;
    }

    @Override
    public void recordShareVisit(String skuId, String goodsId, String distributionId, String shareId) {
        if (CharSequenceUtil.hasBlank(skuId, goodsId, distributionId)) {
            return;
        }
        // 分销员或商品无效时不记录访问
        if (!isShareVisitTargetValid(skuId, goodsId, distributionId)) {
            return;
        }
        String visitorId = UserContext.getUuid();
        if (CharSequenceUtil.isBlank(visitorId)) {
            return;
        }
        AuthUser currentUser = UserContext.getCurrentUser();
        String memberId = currentUser == null ? null : currentUser.getId();
        shareEventService.recordGoodsVisit(distributionId, goodsId, skuId, shareId, visitorId, memberId);
    }

    /**
     * 判断审核状态是否为待审核。
     *
     * @param authStatus 审核状态
     * @return 是否为待审核
     */
    private boolean isApprovedStatusPending(String authStatus) {
        return DistributionGoodsAuthEnum.TOBEAUDITED.name().equals(authStatus);
    }

    /**
     * 为分销商品列表填充平台类目名称。
     *
     * @param records 分销商品列表
     */
    private void enrichPlatformCategoryName(List<DistributionGoodsVO> records) {
        if (records == null || records.isEmpty()) {
            return;
        }
        for (DistributionGoodsVO record : records) {
            record.setPlatformCategoryName(formatPlatformCategoryName(record.getCategoryPath()));
        }
    }

    /**
     * 根据类目路径解析平台三级类目名称。
     *
     * @param categoryPath 类目路径
     * @return 类目名称，无法解析时返回 "-"
     */
    private String formatPlatformCategoryName(String categoryPath) {
        if (CharSequenceUtil.isBlank(categoryPath)) {
            return "-";
        }
        String[] categoryIds = categoryPath.split(",");
        if (categoryIds.length < 3 || CharSequenceUtil.isBlank(categoryIds[2])) {
            return "-";
        }
        Category category = categoryService.getCategoryById(categoryIds[2].trim());
        if (category != null && CharSequenceUtil.isNotBlank(category.getName())) {
            return category.getName();
        }
        return "-";
    }

    /**
     * 校验分享访问目标是否有效（分销员与商品）。
     *
     * @param skuId          SKU ID
     * @param goodsId        商品ID
     * @param distributionId 分销员ID
     * @return 是否有效
     */
    private boolean isShareVisitTargetValid(String skuId, String goodsId, String distributionId) {
        Distribution distribution = distributionService.getById(distributionId);
        // 分销员须已通过审核且未清退
        if (distribution == null
                || !DistributionStatusEnum.PASS.name().equals(distribution.getDistributionStatus())
                || distributionService.isRetreated(distribution.getMemberId())) {
            return false;
        }
        return this.count(Wrappers.lambdaQuery(DistributionGoods.class)
                .eq(DistributionGoods::getSkuId, skuId)
                .eq(DistributionGoods::getGoodsId, goodsId)
                .eq(DistributionGoods::getAuthStatus, DistributionGoodsAuthEnum.PASS.name())) > 0;
    }

    /**
     * 校验分销员与分销商品分享权限。
     *
     * @param skuId          SKU ID
     * @param goodsId        商品ID
     * @param distributionId 分销员ID
     * @return 分享访问上下文
     */
    private ShareAccessContext resolveShareAccess(String skuId, String goodsId, String distributionId) {
        if (CharSequenceUtil.hasBlank(skuId, goodsId, distributionId)) {
            throw new ServiceException(ResultCode.PARAMS_ERROR);
        }
        Distribution distribution = distributionService.getDistribution();
        // 仅允许当前登录分销员分享本人已通过审核的商品
        if (distribution == null
                || !distributionId.equals(distribution.getId())
                || !DistributionStatusEnum.PASS.name().equals(distribution.getDistributionStatus())) {
            throw new ServiceException(ResultCode.DISTRIBUTION_NOT_EXIST);
        }
        DistributionGoods distributionGoods = this.getOne(Wrappers.lambdaQuery(DistributionGoods.class)
                .eq(DistributionGoods::getSkuId, skuId)
                .eq(DistributionGoods::getGoodsId, goodsId)
                .eq(DistributionGoods::getAuthStatus, DistributionGoodsAuthEnum.PASS.name()));
        if (distributionGoods == null) {
            throw new ServiceException(ResultCode.GOODS_NOT_EXIST);
        }
        return new ShareAccessContext(distribution, distributionGoods);
    }

    /**
     * 商品分享访问校验上下文。
     *
     * @param distribution      分销员
     * @param distributionGoods 分销商品
     */
    private record ShareAccessContext(Distribution distribution, DistributionGoods distributionGoods) {
    }

    /**
     * 加载商品 SKU 详情，失败时返回 null。
     *
     * @param goodsId 商品ID
     * @param skuId   SKU ID
     * @return 商品 SKU 信息
     */
    private GoodsSkuVO loadGoodsSkuVO(String goodsId, String skuId) {
        try {
            Map<String, Object> detail = goodsSkuService.getGoodsSkuDetail(goodsId, skuId);
            Object data = detail == null ? null : detail.get("data");
            if (data instanceof GoodsSkuVO) {
                return (GoodsSkuVO) data;
            }
        } catch (ServiceException ignored) {
            // 商品详情不可用时回退分销商品基础信息
        }
        return null;
    }

    /**
     * 汇总分享图片列表（去重）。
     *
     * @param distributionGoods 分销商品
     * @param goodsSkuVO        商品 SKU 信息
     * @return 分享图片 URL 列表
     */
    private List<String> resolveShareImages(DistributionGoods distributionGoods, GoodsSkuVO goodsSkuVO) {
        LinkedHashSet<String> imageSet = new LinkedHashSet<>();
        // 图集优先，再补充分销缩略图与 SKU 缩略图（去重保序）
        if (goodsSkuVO != null && CollUtil.isNotEmpty(goodsSkuVO.getGoodsGalleryList())) {
            goodsSkuVO.getGoodsGalleryList().stream()
                    .filter(CharSequenceUtil::isNotBlank)
                    .forEach(imageSet::add);
        }
        if (CharSequenceUtil.isNotBlank(distributionGoods.getThumbnail())) {
            imageSet.add(distributionGoods.getThumbnail());
        }
        if (goodsSkuVO != null && CharSequenceUtil.isNotBlank(goodsSkuVO.getThumbnail())) {
            imageSet.add(goodsSkuVO.getThumbnail());
        }
        return new ArrayList<>(imageSet);
    }

    /**
     * 构建小程序内分享页面路径。
     *
     * @param skuId          SKU ID
     * @param goodsId        商品ID
     * @param distributionId 分销员ID
     * @return 分享页面路径
     */
    private String buildSharePath(String skuId, String goodsId, String distributionId) {
        return "/pages/product/goods?id=" + skuId + "&goodsId=" + goodsId + "&distributionId=" + distributionId;
    }

    /**
     * 生成分享链接，优先小程序 URL Link，失败时回退 H5。
     *
     * @param skuId          SKU ID
     * @param goodsId        商品ID
     * @param distributionId 分销员ID
     * @param sharePath      分享页面路径
     * @return 分享链接
     */
    private String buildShareLink(String skuId, String goodsId, String distributionId, String sharePath) {
        String query = "id=" + skuId + "&goodsId=" + goodsId + "&distributionId=" + distributionId;
        try {
            String urlLink = wechatMpCodeUtil.createUrlLink("pages/product/goods", query);
            if (CharSequenceUtil.isNotBlank(urlLink)) {
                return urlLink;
            }
        } catch (Exception e) {
            log.warn("生成分销商品小程序链接失败，回退H5链接", e);
        }
        // 小程序 URL Link 不可用时回退 H5 静态页
        return buildH5ShareLink(sharePath);
    }

    /**
     * 根据站点 H5 地址拼接分享链接。
     *
     * @param sharePath 分享页面路径
     * @return H5 分享链接
     */
    private String buildH5ShareLink(String sharePath) {
        Setting setting = settingService.get(SettingEnum.BASE_SETTING.name());
        if (setting == null || CharSequenceUtil.isBlank(setting.getSettingValue())) {
            return sharePath;
        }
        BaseSetting baseSetting = JSONUtil.toBean(setting.getSettingValue(), BaseSetting.class);
        String wapAddress = baseSetting == null ? null : baseSetting.getStaticPageWapAddress();
        if (CharSequenceUtil.isBlank(wapAddress)) {
            return sharePath;
        }
        String base = wapAddress.endsWith("/") ? wapAddress.substring(0, wapAddress.length() - 1) : wapAddress;
        return base + "/#" + sharePath;
    }

}