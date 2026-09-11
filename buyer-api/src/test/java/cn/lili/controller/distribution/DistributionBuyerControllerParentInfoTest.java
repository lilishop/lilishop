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

    private static DistributionSetting visibleSetting(boolean visible) {
        DistributionSetting setting = DistributionSetting.defaultSetting();
        setting.setParentDistributorVisible(visible);
        return setting;
    }

    private static Distribution childWithParent(String parentId) {
        Distribution child = new Distribution();
        child.setId("C1");
        child.setParentDistributionId(parentId);
        return child;
    }

    private static Distribution parentNamed(String memberName) {
        Distribution parent = new Distribution();
        parent.setId("P1");
        parent.setMemberName(memberName);
        return parent;
    }
}
