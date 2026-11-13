package cn.lili.modules.appointment.support;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.utils.SnowFlake;
import cn.lili.modules.appointment.entity.dos.AppointmentGoodsExt;
import cn.lili.modules.appointment.entity.dos.AppointmentSlotTemplate;
import cn.lili.modules.appointment.entity.dos.AppointmentSystemForm;
import cn.lili.modules.appointment.entity.dto.AppointmentGoodsExtSaveDTO;
import cn.lili.modules.appointment.entity.dto.AppointmentSlotTemplateDTO;
import cn.lili.modules.appointment.mapper.AppointmentGoodsExtMapper;
import cn.lili.modules.appointment.mapper.AppointmentSlotTemplateMapper;
import cn.lili.modules.appointment.mapper.AppointmentSystemFormMapper;
import cn.lili.modules.goods.entity.dos.Goods;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.dto.GoodsOperationDTO;
import cn.lili.modules.goods.entity.enums.GoodsTypeEnum;
import cn.lili.modules.goods.entity.vos.GoodsVO;
import cn.lili.modules.goods.mapper.GoodsMapper;
import cn.lili.modules.goods.mapper.GoodsSkuMapper;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import java.util.List;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

/**
 * 预约商品发品及编辑元数据能力，隔离预约订单、支付、履约依赖，统一扩展、时段与表单快照规则。
 * 商品发布和独立配置入口复用相同持久化逻辑，失败在调用方事务中回滚。
 *
 * @author Bulbasaur
 * @since 2026-11-13
 */
@Component
    public class AppointmentGoodsMetadataSupport {
    private final AppointmentGoodsExtMapper extMapper;
    private final AppointmentSlotTemplateMapper slotTemplateMapper;
    private final AppointmentSystemFormMapper systemFormMapper;
    private final GoodsMapper goodsMapper;
    private final GoodsSkuMapper skuMapper;

    /**
     * 注入预约商品元数据和商品基础存储，不依赖预约或商品业务服务。
     *
     * @param extMapper 预约扩展存储
     * @param slotTemplateMapper 时段模板存储
     * @param systemFormMapper 系统表单存储
     * @param goodsMapper 商品存储
     * @param skuMapper SKU存储
     */
    public AppointmentGoodsMetadataSupport(AppointmentGoodsExtMapper extMapper,
            AppointmentSlotTemplateMapper slotTemplateMapper, AppointmentSystemFormMapper systemFormMapper,
            GoodsMapper goodsMapper, GoodsSkuMapper skuMapper) {
        this.extMapper = extMapper;
        this.slotTemplateMapper = slotTemplateMapper;
        this.systemFormMapper = systemFormMapper;
        this.goodsMapper = goodsMapper;
        this.skuMapper = skuMapper;
    }

    /**
     * 校验商家身份、预约商品类型与店铺归属后保存扩展和时段模板。
     *
     * @param dto 预约商品扩展参数
     */
    @Transactional(rollbackFor = Exception.class)
    public void saveGoodsExt(AppointmentGoodsExtSaveDTO dto) {
        AuthUser user = UserContext.getCurrentUser();
        if (user == null || CharSequenceUtil.isBlank(user.getStoreId())) {
            throw new ServiceException(ResultCode.STORE_NOT_LOGIN_ERROR);
        }
        Goods goods = goodsMapper.selectById(dto.getGoodsId());
        if (goods == null || !GoodsTypeEnum.APPOINTMENT_GOODS.name().equals(goods.getGoodsType())) {
            throw new ServiceException(ResultCode.APPOINTMENT_GOODS_TYPE_REQUIRED);
        }
        if (!user.getStoreId().equals(goods.getStoreId())) {
            throw new ServiceException(ResultCode.GOODS_NOT_EXIST_STORE);
        }
        persistGoodsExt(goods, dto);
    }

    /**
     * 发布商品时同步预约配置，未提交预约配置时保留原数据，写入参与商品发布事务。
     *
     * @param goodsId 商品主键
     * @param goodsOperationDTO 商品发布参数
     */
    @Transactional(rollbackFor = Exception.class)
    public void syncGoodsExtFromPublish(String goodsId, GoodsOperationDTO goodsOperationDTO) {
        if (goodsOperationDTO == null
                || !GoodsTypeEnum.APPOINTMENT_GOODS.name().equals(goodsOperationDTO.getGoodsType())) {
            return;
        }
        if (goodsOperationDTO.getAppointmentSettings() == null
                && goodsOperationDTO.getAppointmentSlotTemplate() == null) {
            return;
        }
        Goods goods = goodsMapper.selectById(goodsId);
        if (goods == null) {
            return;
        }
        List<GoodsSku> skus = skuMapper.selectList(new LambdaQueryWrapper<GoodsSku>().eq(GoodsSku::getGoodsId, goodsId)
                .eq(GoodsSku::getStoreId, goods.getStoreId()).eq(GoodsSku::getDeleteFlag, false));
        AppointmentGoodsExtSaveDTO saveDTO = AppointmentGoodsPublishSupport.toSaveDto(
                goodsId,
                goodsOperationDTO.getAppointmentSettings(),
                goodsOperationDTO.getAppointmentSlotTemplate(),
                goodsOperationDTO.getSkuList(),
                skus);
        persistGoodsExt(goods, saveDTO);
    }

    /**
     * 组装商家预约商品编辑回显，复用发品规则转换扩展、模板及SKU容量。
     *
     * @param goodsVO 商品详情视图
     */
    public void enrichGoodsVoForSeller(GoodsVO goodsVO) {
        if (goodsVO == null || !GoodsTypeEnum.APPOINTMENT_GOODS.name().equals(goodsVO.getGoodsType())) {
            return;
        }
        AppointmentGoodsExt ext = extMapper.selectOne(new LambdaQueryWrapper<AppointmentGoodsExt>()
                .eq(AppointmentGoodsExt::getGoodsId, goodsVO.getId())
                .eq(AppointmentGoodsExt::getDeleteFlag, false));
        List<AppointmentSlotTemplate> templates = slotTemplateMapper.selectList(
                new LambdaQueryWrapper<AppointmentSlotTemplate>()
                        .eq(AppointmentSlotTemplate::getGoodsId, goodsVO.getId())
                        .eq(AppointmentSlotTemplate::getDeleteFlag, false)
                        .orderByAsc(AppointmentSlotTemplate::getSortOrder));
        goodsVO.setAppointmentSettings(AppointmentGoodsPublishSupport.toSettingsMap(ext));
        goodsVO.setAppointmentSlotTemplate(AppointmentGoodsPublishSupport.toSlotTemplateMap(ext, templates));
        AppointmentGoodsPublishSupport.attachSkuSlotCapacities(goodsVO, templates);
    }

    /**
     * 保存预约扩展及表单快照，非空时段模板覆盖旧模板，空模板保持原值。
     *
     * @param goods 商品实体
     * @param dto 预约配置
     */
    private void persistGoodsExt(Goods goods, AppointmentGoodsExtSaveDTO dto) {
        AppointmentGoodsExt ext = extMapper.selectOne(new LambdaQueryWrapper<AppointmentGoodsExt>()
                .eq(AppointmentGoodsExt::getGoodsId, dto.getGoodsId())
                .eq(AppointmentGoodsExt::getDeleteFlag, false));
        if (ext == null) {
            ext = BeanUtil.copyProperties(dto, AppointmentGoodsExt.class);
            ext.setId(SnowFlake.getIdStr());
            ext.setStoreId(goods.getStoreId());
            ext.setDeleteFlag(false);
            fillSystemFormSnapshot(ext, goods.getStoreId());
            extMapper.insert(ext);
        } else {
            BeanUtil.copyProperties(dto, ext, "id", "goodsId", "storeId");
            fillSystemFormSnapshot(ext, goods.getStoreId());
            extMapper.updateById(ext);
        }
        if (CollUtil.isNotEmpty(dto.getSlotTemplates())) {
            slotTemplateMapper.delete(new LambdaQueryWrapper<AppointmentSlotTemplate>()
                    .eq(AppointmentSlotTemplate::getGoodsId, dto.getGoodsId()));
            for (AppointmentSlotTemplateDTO templateDTO : dto.getSlotTemplates()) {
                AppointmentSlotTemplate template = BeanUtil.copyProperties(templateDTO, AppointmentSlotTemplate.class);
                template.setId(SnowFlake.getIdStr());
                template.setGoodsId(dto.getGoodsId());
                template.setStoreId(goods.getStoreId());
                template.setDeleteFlag(false);
                if (template.getEnabled() == null) {
                    template.setEnabled(true);
                }
                if (template.getCapacity() == null) {
                    template.setCapacity(0);
                }
                slotTemplateMapper.insert(template);
            }
        }
    }

    /**
     * 校验系统表单归属后保存清理后的表单快照，无表单关联时清空历史快照。
     *
     * @param ext 预约扩展实体
     * @param storeId 所属店铺主键
     */
    private void fillSystemFormSnapshot(AppointmentGoodsExt ext, String storeId) {
        if (ext == null) {
            return;
        }
        if (CharSequenceUtil.isBlank(ext.getSystemFormId())) {
            ext.setSystemFormId(null);
            ext.setSystemFormSnapshot(null);
            return;
        }
        AppointmentSystemForm form = systemFormMapper.selectById(ext.getSystemFormId());
        if (form == null || Boolean.TRUE.equals(form.getDeleteFlag())
                || (CharSequenceUtil.isNotBlank(storeId) && !storeId.equals(form.getStoreId()))) {
            throw new ServiceException(ResultCode.APPOINTMENT_SYSTEM_FORM_NOT_FOUND);
        }
        ext.setSystemFormSnapshot(AppointmentFormAnswerSupport.snapshotSchema(form.getFormSchema()));
    }
}
