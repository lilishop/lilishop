package cn.lili.modules.appointment.support;

import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.modules.appointment.entity.dos.AppointmentGoodsExt;
import cn.lili.modules.appointment.entity.dos.AppointmentSlotTemplate;
import cn.lili.modules.appointment.entity.dos.AppointmentSystemForm;
import cn.lili.modules.appointment.entity.dto.AppointmentGoodsExtSaveDTO;
import cn.lili.modules.appointment.entity.dto.AppointmentSlotTemplateDTO;
import cn.lili.modules.appointment.mapper.AppointmentGoodsExtMapper;
import cn.lili.modules.appointment.mapper.AppointmentSlotTemplateMapper;
import cn.lili.modules.appointment.mapper.AppointmentSystemFormMapper;
import cn.lili.modules.goods.entity.dos.Goods;
import cn.lili.modules.goods.entity.dto.GoodsOperationDTO;
import cn.lili.modules.goods.entity.enums.GoodsTypeEnum;
import cn.lili.modules.goods.mapper.GoodsMapper;
import cn.lili.modules.goods.mapper.GoodsSkuMapper;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

/**
 * 预约发品元数据拆分回归测试，覆盖店铺归属、系统表单快照与时段模板默认值，不连接数据库。
 *
 * @author Bulbasaur
 * @since 2026-11-13
 */
class AppointmentGoodsMetadataSupportTest {
    private final AppointmentGoodsExtMapper extensions = mock(AppointmentGoodsExtMapper.class);
    private final AppointmentSlotTemplateMapper templates = mock(AppointmentSlotTemplateMapper.class);
    private final AppointmentSystemFormMapper forms = mock(AppointmentSystemFormMapper.class);
    private final GoodsMapper goods = mock(GoodsMapper.class);
    private final GoodsSkuMapper skus = mock(GoodsSkuMapper.class);
    private final AppointmentGoodsMetadataSupport support =
            new AppointmentGoodsMetadataSupport(extensions, templates, forms, goods, skus);

    /**
     * 构造指定店铺下的预约商品。
     *
     * @param storeId 商品所属店铺主键
     * @return 预约商品
     */
    private Goods goods(String storeId) {
        Goods goods = new Goods();
        goods.setId("goods");
        goods.setStoreId(storeId);
        goods.setGoodsType(GoodsTypeEnum.APPOINTMENT_GOODS.name());
        return goods;
    }

    /** 验证跨店铺保存预约配置被拒绝，且不写扩展或模板。 */
    @Test
    void rejectsAnotherStoresGoodsBeforeWriting() {
        when(goods.selectById("goods")).thenReturn(goods("other"));
        AppointmentGoodsExtSaveDTO dto = new AppointmentGoodsExtSaveDTO();
        dto.setGoodsId("goods");
        try (var users = mockStatic(UserContext.class)) {
            users.when(UserContext::getCurrentUser).thenReturn(AuthUser.builder().storeId("store").build());
            assertThatThrownBy(() -> support.saveGoodsExt(dto)).isInstanceOf(ServiceException.class);
        }
        verifyNoInteractions(extensions, templates, forms);
    }

    /** 验证空表单关联会清空快照，新模板默认启用且容量为零，并绑定商品所属店铺。 */
    @Test
    void preservesSnapshotClearingAndTemplateDefaults() {
        when(goods.selectById("goods")).thenReturn(goods("store"));
        AppointmentGoodsExtSaveDTO dto = new AppointmentGoodsExtSaveDTO();
        dto.setGoodsId("goods");
        dto.setSlotTemplates(List.of(new AppointmentSlotTemplateDTO()));
        try (var users = mockStatic(UserContext.class)) {
            users.when(UserContext::getCurrentUser).thenReturn(AuthUser.builder().storeId("store").build());
            support.saveGoodsExt(dto);
        }
        ArgumentCaptor<AppointmentGoodsExt> ext = ArgumentCaptor.forClass(AppointmentGoodsExt.class);
        verify(extensions).insert(ext.capture());
        assertThat(ext.getValue().getSystemFormId()).isNull();
        assertThat(ext.getValue().getSystemFormSnapshot()).isNull();
        assertThat(ext.getValue().getStoreId()).isEqualTo("store");
        ArgumentCaptor<AppointmentSlotTemplate> template = ArgumentCaptor.forClass(AppointmentSlotTemplate.class);
        verify(templates).insert(template.capture());
        assertThat(template.getValue().getGoodsId()).isEqualTo("goods");
        assertThat(template.getValue().getStoreId()).isEqualTo("store");
        assertThat(template.getValue().getEnabled()).isTrue();
        assertThat(template.getValue().getCapacity()).isZero();
    }

    /** 验证引用其他店铺表单时拒绝保存，禁止将其表单快照写入商品。 */
    @Test
    void rejectsAnotherStoresFormSnapshot() {
        when(goods.selectById("goods")).thenReturn(goods("store"));
        AppointmentSystemForm form = new AppointmentSystemForm();
        form.setStoreId("other");
        when(forms.selectById("form")).thenReturn(form);
        AppointmentGoodsExtSaveDTO dto = new AppointmentGoodsExtSaveDTO();
        dto.setGoodsId("goods");
        dto.setSystemFormId("form");
        try (var users = mockStatic(UserContext.class)) {
            users.when(UserContext::getCurrentUser).thenReturn(AuthUser.builder().storeId("store").build());
            assertThatThrownBy(() -> support.saveGoodsExt(dto)).isInstanceOf(ServiceException.class);
        }
        verify(extensions, never()).insert(any(AppointmentGoodsExt.class));
        verifyNoInteractions(templates);
    }

    /** 验证发品没有提交预约配置时不覆盖原有扩展或时段模板。 */
    @Test
    void omittedPublishSettingsDoNotOverwriteExistingMetadata() {
        GoodsOperationDTO dto = new GoodsOperationDTO();
        dto.setGoodsType(GoodsTypeEnum.APPOINTMENT_GOODS.name());
        support.syncGoodsExtFromPublish("goods", dto);
        verifyNoInteractions(goods, skus, extensions, templates, forms);
    }
}
