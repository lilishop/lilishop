package cn.lili.controller.distribution;

import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.distribution.entity.dos.Distribution;
import cn.lili.modules.distribution.entity.vos.DistributionParentInfoVO;
import cn.lili.modules.distribution.service.DistributionService;
import cn.lili.modules.distribution.support.DistributionSettingSupport;
import cn.lili.modules.system.entity.dto.DistributionSetting;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * GET /parent 返回包装 VO：开关与上级资料分离，关闭时仍回上级字段。
 *
 * @author Mike
 * @date 2026-09-11
 */
@ExtendWith(MockitoExtension.class)
class DistributionBuyerControllerParentInfoTest {

    @InjectMocks
    private DistributionBuyerController controller;

    @Mock
    private DistributionService distributionService;

    @Mock
    private DistributionSettingSupport distributionSettingSupport;

    /**
     * 验证开启展示时返回上级分销员信息。
     */
    @Test
    void parentInfoWhenVisibleTrueReturnsParent() {
        when(distributionSettingSupport.getSetting()).thenReturn(visibleSetting(true));
        when(distributionService.getDistribution()).thenReturn(childWithParent("P1"));
        when(distributionService.getById("P1")).thenReturn(parentNamed("张三"));

        ResultMessage<DistributionParentInfoVO> message = controller.parentInfo();

        DistributionParentInfoVO vo = message.getResult();
        assertThat(vo.getParentDistributorVisible()).isTrue();
        assertThat(vo.getParentDistributionId()).isEqualTo("P1");
        assertThat(vo.getParentNickname()).isEqualTo("张三");
    }

    /**
     * 验证关闭展示开关时接口仍返回上级信息。
     */
    @Test
    void parentInfoWhenVisibleFalseStillReturnsParent() {
        when(distributionSettingSupport.getSetting()).thenReturn(visibleSetting(false));
        when(distributionService.getDistribution()).thenReturn(childWithParent("P1"));
        when(distributionService.getById("P1")).thenReturn(parentNamed("张三"));

        ResultMessage<DistributionParentInfoVO> message = controller.parentInfo();

        DistributionParentInfoVO vo = message.getResult();
        assertThat(vo.getParentDistributorVisible()).isFalse();
        assertThat(vo.getParentDistributionId()).isEqualTo("P1");
        assertThat(vo.getParentNickname()).isEqualTo("张三");
    }

    /**
     * 验证没有上级时返回空的上级信息字段。
     */
    @Test
    void parentInfoWhenVisibleTrueAndNoParentReturnsEmptyParentFields() {
        when(distributionSettingSupport.getSetting()).thenReturn(visibleSetting(true));
        Distribution child = new Distribution();
        child.setId("C1");
        when(distributionService.getDistribution()).thenReturn(child);

        ResultMessage<DistributionParentInfoVO> message = controller.parentInfo();

        DistributionParentInfoVO vo = message.getResult();
        assertThat(vo.getParentDistributorVisible()).isTrue();
        assertThat(vo.getParentDistributionId()).isNull();
        assertThat(vo.getParentNickname()).isNull();
        verify(distributionService, never()).getById(anyString());
    }

    /**
     * 验证非分销员会员仍获得展示开关信息。
     */
    @Test
    void parentInfoWhenNotDistributorStillReturnsVisible() {
        when(distributionSettingSupport.getSetting()).thenReturn(visibleSetting(true));
        when(distributionService.getDistribution()).thenReturn(null);

        ResultMessage<DistributionParentInfoVO> message = controller.parentInfo();

        DistributionParentInfoVO vo = message.getResult();
        assertThat(vo.getParentDistributorVisible()).isTrue();
        assertThat(vo.getParentDistributionId()).isNull();
        assertThat(vo.getParentNickname()).isNull();
        verify(distributionSettingSupport).getSetting();
        verify(distributionService, never()).getById(anyString());
    }

    /**
     * 验证上级记录缺失时保留上级ID且不填昵称。
     */
    @Test
    void parentInfoWhenParentRecordMissingKeepsIdWithoutNickname() {
        when(distributionSettingSupport.getSetting()).thenReturn(visibleSetting(true));
        when(distributionService.getDistribution()).thenReturn(childWithParent("P1"));
        when(distributionService.getById("P1")).thenReturn(null);

        ResultMessage<DistributionParentInfoVO> message = controller.parentInfo();

        DistributionParentInfoVO vo = message.getResult();
        assertThat(vo.getParentDistributorVisible()).isTrue();
        assertThat(vo.getParentDistributionId()).isEqualTo("P1");
        assertThat(vo.getParentNickname()).isNull();
    }

    /**
     * 验证未配置上级信息展示开关时使用默认开启值。
     */
    @Test
    void parentInfoWhenSettingFieldMissingDefaultsTrue() {
        DistributionSetting setting = new DistributionSetting();
        setting.setIsOpen(true);
        assertThat(setting.getParentDistributorVisible()).isNull();
        setting.applyDefaults();
        when(distributionSettingSupport.getSetting()).thenReturn(setting);
        when(distributionService.getDistribution()).thenReturn(null);

        ResultMessage<DistributionParentInfoVO> message = controller.parentInfo();

        assertThat(message.getResult().getParentDistributorVisible()).isTrue();
        assertThat(message.getResult().getParentDistributionId()).isNull();
    }

    /**
     * 构造指定上级信息展示开关的分销配置。
     *
     * @param visible 是否展示上级信息
     * @return 指定上级信息展示开关的分销配置
     */
    private static DistributionSetting visibleSetting(boolean visible) {
        DistributionSetting setting = DistributionSetting.defaultSetting();
        setting.setParentDistributorVisible(visible);
        return setting;
    }

    /**
     * 构造绑定指定上级的下级分销员。
     *
     * @param parentId 上级ID
     * @return 绑定指定上级的下级分销员
     */
    private static Distribution childWithParent(String parentId) {
        Distribution child = new Distribution();
        child.setId("C1");
        child.setParentDistributionId(parentId);
        return child;
    }

    /**
     * 构造指定昵称的上级分销员。
     *
     * @param memberName 会员名称
     * @return 指定昵称的上级分销员
     */
    private static Distribution parentNamed(String memberName) {
        Distribution parent = new Distribution();
        parent.setId("P1");
        parent.setMemberName(memberName);
        return parent;
    }
}
