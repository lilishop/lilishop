package cn.lili.modules.goods.entity.dto;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import tools.jackson.databind.exc.InvalidFormatException;
import tools.jackson.databind.json.JsonMapper;

import java.time.OffsetDateTime;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

class GoodsMarketScheduleDTOTest {

    private final JsonMapper mapper = JsonMapper.builder().build();

    /**
     * 验证前端日期时间字符串按东八区解析，并保留商品ID及计划类型
     *
     * @param time 前端提交的日期时间，格式为yyyy-MM-dd HH:mm:ss
     */
    @ParameterizedTest
    @ValueSource(strings = {"2026-11-12 00:00:00", "2026-11-12 18:30:45"})
    void acceptsFrontendDateTimeInChinaTimeZone(String time) {
        String json = "{\"goodsIds\":[\"goods-1\"],\"status\":\"UPPER\",\"triggerTime\":\"" + time + "\"}";

        GoodsMarketScheduleDTO dto = mapper.readValue(json, GoodsMarketScheduleDTO.class);

        assertThat(dto.getTriggerTime().toInstant())
                .isEqualTo(OffsetDateTime.parse(time.replace(' ', 'T') + "+08:00").toInstant());
        assertThat(dto.getGoodsIds()).containsExactly("goods-1");
        assertThat(dto.getStatus()).isEqualTo("UPPER");
    }

    /**
     * 验证触发时间仍兼容毫秒时间戳，解析后时间值保持不变
     */
    @Test
    void stillAcceptsEpochMilliseconds() {
        long timestamp = OffsetDateTime.parse("2026-11-12T00:00:00+08:00").toInstant().toEpochMilli();
        GoodsMarketScheduleDTO dto = mapper.readValue("{\"triggerTime\":" + timestamp + "}", GoodsMarketScheduleDTO.class);

        assertThat(dto.getTriggerTime().getTime()).isEqualTo(timestamp);
    }

    /**
     * 验证非法日期字符串及不存在的日期被拒绝，不被宽松转换为其他日期
     *
     * @param time 非法日期时间字符串
     */
    @ParameterizedTest
    @ValueSource(strings = {"not-a-date", "2026-02-30 00:00:00"})
    void rejectsInvalidDates(String time) {
        assertThrows(InvalidFormatException.class,
                () -> mapper.readValue("{\"triggerTime\":\"" + time + "\"}", GoodsMarketScheduleDTO.class));
    }
}
