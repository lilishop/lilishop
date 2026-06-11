package cn.lili.modules.file.plugin.impl;

import cn.lili.common.exception.ServiceException;
import cn.lili.modules.system.entity.dto.OssSetting;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.ByteArrayInputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collections;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class LocalFilePluginTest {

    @TempDir
    Path tempDir;

    @Test
    void inputStreamUploadShouldSaveFileAndReturnAccessUrl() throws Exception {
        OssSetting ossSetting = new OssSetting();
        ossSetting.setLocalFilePath(tempDir.toString());
        ossSetting.setLocalFileUrlPrefix("/files");
        LocalFilePlugin plugin = new LocalFilePlugin(ossSetting);

        String url = plugin.inputStreamUpload(
                new ByteArrayInputStream("hello".getBytes(StandardCharsets.UTF_8)),
                "MANAGER/default/demo.txt",
                "text/plain"
        );

        assertEquals("/files/MANAGER/default/demo.txt", url);
        assertEquals("hello", Files.readString(tempDir.resolve("MANAGER/default/demo.txt")));
    }

    @Test
    void deleteFileShouldRemoveLocalFile() throws Exception {
        OssSetting ossSetting = new OssSetting();
        ossSetting.setLocalFilePath(tempDir.toString());
        LocalFilePlugin plugin = new LocalFilePlugin(ossSetting);
        Path file = tempDir.resolve("STORE/1/a.txt");
        Files.createDirectories(file.getParent());
        Files.writeString(file, "remove");

        plugin.deleteFile(Collections.singletonList("STORE/1/a.txt"));

        assertFalse(Files.exists(file));
    }

    @Test
    void inputStreamUploadShouldRejectPathTraversal() {
        OssSetting ossSetting = new OssSetting();
        ossSetting.setLocalFilePath(tempDir.toString());
        LocalFilePlugin plugin = new LocalFilePlugin(ossSetting);

        assertThrows(ServiceException.class, () -> plugin.inputStreamUpload(
                new ByteArrayInputStream("bad".getBytes(StandardCharsets.UTF_8)),
                "../bad.txt",
                "text/plain"
        ));
        assertTrue(Files.notExists(tempDir.getParent().resolve("bad.txt")));
    }
}
