package cn.lili.modules.system.entity.dto;

import cn.hutool.json.JSONUtil;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 分类页设置 DTO:默认值与缺键容错。
 */
class CategoryPageSettingTest {

    /**
     * 验证分类页默认使用模板一且所有展示字段开启。
     */
    @Test
    void defaults_are_template1_and_all_fields_true() {
        CategoryPageSetting s = new CategoryPageSetting();
        assertThat(s.getTemplate()).isEqualTo(1);
        assertThat(s.getShowFields()).isNotNull();
        assertThat(s.getShowFields().getLinePrice()).isTrue();
    }

    /**
     * 验证分类页配置缺少字段时使用默认值。
     */
    @Test
    void missing_keys_fall_back_to_defaults() {
        CategoryPageSetting s = JSONUtil.toBean("{\"template\":2}", CategoryPageSetting.class);
        assertThat(s.getTemplate()).isEqualTo(2);
        assertThat(s.getShowFields().getCart()).isTrue();
    }
}
