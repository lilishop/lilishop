package cn.lili.modules.distribution.serviceimpl;

import cn.hutool.json.JSONUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.distribution.entity.dos.DistributionGoods;
import cn.lili.modules.distribution.entity.dto.DistributionFeaturedGoodsSettingSaveDTO;
import cn.lili.modules.distribution.entity.enums.DistributionFeaturedMatchRuleEnum;
import cn.lili.modules.distribution.entity.enums.DistributionFeaturedSourceModeEnum;
import cn.lili.modules.distribution.service.DistributionGoodsService;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.DistributionFeaturedGoodsSetting;
import cn.lili.modules.system.service.SettingService;
import com.baomidou.mybatisplus.core.MybatisConfiguration;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import org.apache.ibatis.builder.MapperBuilderAssistant;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * 自定义主推商品保存按 displayCount 硬拦截。
 *
 * @author Mike
 * @date 2026-09-13
 */
class DistributionFeaturedGoodsServiceImplSaveSettingTest {

    private DistributionFeaturedGoodsServiceImpl service;
    private SettingService settingService;
    private DistributionGoodsService distributionGoodsService;

    @BeforeAll
    static void initMybatisEntityCache() {
        MapperBuilderAssistant assistant = new MapperBuilderAssistant(new MybatisConfiguration(), "");
        TableInfoHelper.initTableInfo(assistant, DistributionGoods.class);
    }

    @BeforeEach
    void setUp() {
        service = new DistributionFeaturedGoodsServiceImpl();
        settingService = mock(SettingService.class);
        distributionGoodsService = mock(DistributionGoodsService.class);
        ReflectionTestUtils.setField(service, "settingService", settingService);
        ReflectionTestUtils.setField(service, "distributionGoodsService", distributionGoodsService);
        when(settingService.getById(any())).thenReturn(null);
        when(settingService.saveUpdate(any(Setting.class))).thenReturn(true);
    }

    @Test
    void saveSetting_rejectsCustomWhenSkuCountExceedsDisplayCountFive() {
        DistributionFeaturedGoodsSettingSaveDTO dto = customDto(5, skuIds(6));

        assertThatThrownBy(() -> service.saveSetting(dto))
                .isInstanceOf(ServiceException.class)
                .extracting(ex -> ((ServiceException) ex).getMsg())
                .asString()
                .contains("最多选择5个");
        verify(settingService, never()).saveUpdate(any(Setting.class));
    }

    @Test
    void saveSetting_persistsFiveCustomSkuIdsWhenDisplayCountIsFive() {
        List<String> skuIds = skuIds(5);
        stubPassedGoods(skuIds);

        service.saveSetting(customDto(5, skuIds));

        assertThat(savedSetting().getSkuIds()).containsExactlyElementsOf(skuIds);
    }

    @Test
    void saveSetting_persistsTenCustomSkuIdsWhenDisplayCountIsTen() {
        List<String> skuIds = skuIds(10);
        stubPassedGoods(skuIds);

        service.saveSetting(customDto(10, skuIds));

        assertThat(savedSetting().getSkuIds()).containsExactlyElementsOf(skuIds);
    }

    @Test
    void saveSetting_acceptsCustomAfterNormalizingBlankAndDuplicateSkuIds() {
        List<String> rawSkuIds = new ArrayList<>();
        rawSkuIds.add("sku-1");
        rawSkuIds.add("");
        rawSkuIds.add(" sku-1 ");
        rawSkuIds.add("sku-2");
        rawSkuIds.add("sku-3");
        rawSkuIds.add("sku-4");
        rawSkuIds.add("sku-5");
        stubPassedGoods(List.of("sku-1", "sku-2", "sku-3", "sku-4", "sku-5"));

        service.saveSetting(customDto(5, rawSkuIds));

        assertThat(savedSetting().getSkuIds()).containsExactly("sku-1", "sku-2", "sku-3", "sku-4", "sku-5");
    }

    @Test
    void saveSetting_rejectsCustomWhenSkuIdsEmpty() {
        assertThatThrownBy(() -> service.saveSetting(customDto(5, new ArrayList<>())))
                .isInstanceOf(ServiceException.class)
                .extracting(ex -> ((ServiceException) ex).getMsg())
                .asString()
                .contains("请至少选择一个自定义主推商品");
        verify(settingService, never()).saveUpdate(any(Setting.class));
    }

    @Test
    void saveSetting_clearsSkuIdsInAutoModeEvenWhenMoreThanFiveSubmitted() {
        DistributionFeaturedGoodsSettingSaveDTO dto = new DistributionFeaturedGoodsSettingSaveDTO();
        dto.setSourceMode(DistributionFeaturedSourceModeEnum.AUTO.name());
        dto.setMatchRule(DistributionFeaturedMatchRuleEnum.HIGHEST_COMMISSION.name());
        dto.setDisplayCount(5);
        dto.setSkuIds(skuIds(8));

        service.saveSetting(dto);

        assertThat(savedSetting().getSkuIds()).isEmpty();
        verify(distributionGoodsService, never()).list(any(Wrapper.class));
    }

    @Test
    void validateSetting_rejectsAfterDistinctCountExceedsDisplayCount() {
        DistributionFeaturedGoodsSettingSaveDTO dto = customDto(5, skuIds(6));
        dto.getSkuIds().add("sku-1");
        dto.getSkuIds().add(" ");

        assertThatThrownBy(() -> ReflectionTestUtils.invokeMethod(service, "validateSetting", dto))
                .isInstanceOf(ServiceException.class)
                .extracting(ex -> ((ServiceException) ex).getMsg())
                .asString()
                .contains("最多选择5个");
    }

    private void stubPassedGoods(List<String> skuIds) {
        List<DistributionGoods> goods = skuIds.stream().map(skuId -> {
            DistributionGoods item = new DistributionGoods();
            item.setSkuId(skuId);
            return item;
        }).collect(Collectors.toList());
        when(distributionGoodsService.list(any(Wrapper.class))).thenReturn(goods);
    }

    private DistributionFeaturedGoodsSetting savedSetting() {
        ArgumentCaptor<Setting> captor = ArgumentCaptor.forClass(Setting.class);
        verify(settingService).saveUpdate(captor.capture());
        return JSONUtil.toBean(captor.getValue().getSettingValue(), DistributionFeaturedGoodsSetting.class);
    }

    private static DistributionFeaturedGoodsSettingSaveDTO customDto(int displayCount, List<String> skuIds) {
        DistributionFeaturedGoodsSettingSaveDTO dto = new DistributionFeaturedGoodsSettingSaveDTO();
        dto.setSourceMode(DistributionFeaturedSourceModeEnum.CUSTOM.name());
        dto.setDisplayCount(displayCount);
        dto.setSkuIds(skuIds);
        return dto;
    }

    private static List<String> skuIds(int count) {
        return IntStream.rangeClosed(1, count)
                .mapToObj(i -> "sku-" + i)
                .collect(Collectors.toList());
    }
}
