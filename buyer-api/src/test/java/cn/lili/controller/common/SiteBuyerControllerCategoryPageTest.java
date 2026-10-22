package cn.lili.controller.common;

import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * GET /buyer/common/site/category-page 返回分类页设置。
 */
@ExtendWith(MockitoExtension.class)
class SiteBuyerControllerCategoryPageTest {

    @InjectMocks
    private SiteBuyerController controller;

    @Mock
    private SettingService settingService;

    @Test
    void categoryPageSettingReturnsStoredSetting() {
        Setting setting = new Setting();
        setting.setId(SettingEnum.CATEGORY_PAGE_SETTING.name());
        setting.setSettingValue("{\"template\":2}");
        when(settingService.get(SettingEnum.CATEGORY_PAGE_SETTING.name())).thenReturn(setting);

        ResultMessage<Object> message = controller.categoryPageSetting();

        verify(settingService).get("CATEGORY_PAGE_SETTING");
        assertThat(message.getResult()).isEqualTo(setting);
    }

    @Test
    void categoryPageSettingReturnsNullWhenNotConfigured() {
        when(settingService.get(SettingEnum.CATEGORY_PAGE_SETTING.name())).thenReturn(null);

        ResultMessage<Object> message = controller.categoryPageSetting();

        assertThat(message.getResult()).isNull();
    }
}
